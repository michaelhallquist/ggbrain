#' @title R6 class for a single layer of a ggbrain panel using outline geom
#' @details 
#'   Note that this class is exported only for power users and rarely needs to be called directly
#'     in typical use of the package. Instead, look at `geom_outline()`.
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes
#' @importFrom Matrix sparseMatrix
#' @importFrom imager as.cimg erode_square isoblur boundary px.circle
#' @importFrom dplyr group_by group_keys group_split
#' @importFrom rlang as_label
#' @return a `ggbrain_layer_outline` R6 class with fields related to a brain visual layer (relates to `geom_outline`)
#' @export
ggbrain_layer_outline <- R6::R6Class(
  classname = "ggbrain_layer_outline",
  inherit = ggbrain_layer,
  private = list(
    pvt_size = NULL,
    pvt_group_column = NULL, # handles case where we want to control how outlines are defined/grouped (e.g., by region versus by label)
    pvt_dil_ero = 0L, # number of pixels to dilate or erode outline

    get_plot_data = function() {
      if (is.null(self$size)) return(NULL) # if size is NULL, there is nothing to plot

      data_list <- super$get_plot_data() # general data

      # convert all relevant data.frames to outlines
      data_list[c("df", "df_pos", "df_neg")] <- lapply(
        data_list[c("df", "df_pos", "df_neg")], private$slice_to_outline, private$pvt_group_column, private$pvt_blur_edge, private$pvt_dil_ero
      )

      return(data_list)
    },

    # function that traces the outline of the slice, grouped by image
    slice_to_outline = function(df, group_cols = NULL, blur_sigma = 0.9, dil_ero=0) {
      if (is.null(df)) return(NULL) # don't attempt to outline an empty object

      # always drop NAs before proceeding with outline since only non-NA points can contribute
      max_dim1 <- max(df$dim1) # extent of x axis (before na.omit)
      max_dim2 <- max(df$dim2)
      df <- subset(df, !is.na(value))
      attr(df, "dim1") <- max_dim1
      attr(df, "dim2") <- max_dim2

      if (nrow(df) == 0L) return(df) # skip out if no valid rows

      # if we have grouping factors, split on these and get outlines for each component
      if (!is.null(group_cols)) {
        stopifnot(all(group_cols %in% names(df)))
        dfg <- df %>% dplyr::group_by(across(all_of(group_cols)))
        df_split <- dfg %>% dplyr::group_split(.keep = TRUE)
        df_keys <- dfg %>% dplyr::group_keys()
        by_roi <- TRUE
      } else {
        df_split <- list(df)
        by_roi <- FALSE
      }

      df_melt <- dplyr::bind_rows(lapply(seq_along(df_split), function(ii) {
        dd <- df_split[[ii]]
        # convert to a 0/1 matrix where 1s denote present voxels
        slc_mat <- as.matrix(Matrix::sparseMatrix(i = dd$dim1, j = dd$dim2, x = 1, dims = c(max_dim1, max_dim2)))
        slc_img <- imager::as.cimg(slc_mat) # convert to cimg object

        # follows this logic: https://stackoverflow.com/questions/29878844/how-do-i-find-the-perimeter-of-objects-in-a-binary-image
        # er <- imager::erode_square(slc_img, 3) # erode by very small square
        # res1 <- abs(er - slc_img) # retained images become -1 -- not sure why this is needed...
        # res <- slc_img - er # retained images become -1
        
        # handle dilation and erosion
        if (dil_ero > 0L) {
          ker <- imager::px.circle(dil_ero) # circular kernel
          slc_img <- imager::dilate(slc_img, ker)
        } else if (dil_ero < 0L) {
          ker <- imager::px.circle(abs(dil_ero)) # circular kernel
          slc_img <- imager::erode(slc_img, ker)
        }

        # simpler approach of using imager boundary function
        b <- imager::boundary(slc_img, depth = private$pvt_size)
        if (!is.null(blur_sigma)) {
          orig <- imager::as.cimg(b)
          res <- imager::isoblur(orig, sigma = blur_sigma)
          res[b] <- 1 # always keep original boundary as alpha 1.0 (don't blur in 0 pixels)
        } else {
          res <- imager::as.cimg(b) # just convert boundary into 1/0 image
        }

        # scale antialiasing of outline by base alpha transparency requested
        base_alpha <- ifelse(is.null(private$pvt_alpha), 1, private$pvt_alpha)

        # need to protect value in AsIs I() notation to get simple alpha mapping in ggplot2
        # https://www.andrea-rau.com/post/alpha-transparency-in-ggplot2/
        # isoblur generates some tiny values! If alpha is less than 5%, treat it as NA/empty
        ret <- mat2df(as.matrix(res)) %>%
          mutate(
            value = if_else(value < .05, 0, value),
            # rescale_max is obviated by approach of keeping the pixset values at 1.0 in the post-blur image
            #value = !!base_alpha*scales::rescale_max(value), # sets the max to 1.0 -- blur can sometimes have values ~0.96
            value = !!base_alpha*value,
            alpha = I(value)
          )

        # copy across factor levels for categorical data
        if (isTRUE(by_roi)) ret <- ret %>% dplyr::bind_cols(df_keys[ii,,drop=FALSE])
        
        zvals <- ret$value < 1e-6
        ret[zvals, c("value", group_cols)] <- NA # set value==0 pixels to NA
        
        return(ret)
      }))

      # handle alpha layering -- need to convert to an alpha mapping internally to get antialiasing to work
      if (!is.null(blur_sigma)) {
        private$pvt_alpha_column <- "alpha" # set alpha aes mapping
        private$pvt_alpha <- NULL # unset the fixed alpha mapping since it has been blended with an alpha mapping
      }

      attr(df_melt, "dim1") <- max_dim1
      attr(df_melt, "dim2") <- max_dim2

      # geom_raster does not render transparent NAs when there is not a scale_fill_* object -- so if it's a simple
      # fixed-color outline, the NAs will render in that color, too. Hence, for outlines, we must drop NAs.
      # In v0.8, this is now handled at the add_raster level so that fixed fills for geom_brain work, too
      #return(df_melt %>% na.omit())
      return(df_melt)
    }
  ),
  active = list(
    #' @field mapping the ggplot2 aesthetic mapping between the data columns and the display
    #' @details To set mapping, you must provide a ggplot2 aes() object. A geom_outline() layer requires
    #'   an `outline` aesthetic mapping, which controls the color of outlines drawn around regions.
    mapping = function(value) {
      if (missing(value)) {
        private$pvt_mapping
      } else {
        checkmate::assert_class(value, "uneval")
        private$pvt_mapping <- value
        if (!is.null(value$fill)) {
          stop("geom_outline() objects only support an outline aesthetic mapping: aes(outline=<value>)")
        }

        if (is.null(value$outline)) {
          private$pvt_fill_column <- NULL
        } else {
          private$pvt_fill_column <- all.vars(value$outline) # pull out the outline column from aes
          if (length(private$pvt_fill_column) > 1L) stop("Cannot pass multiple columns as a fill mapping")

          # if the user modifies the fill column such as as.factor(), we need to carry through the expression to geom_raster
          # inside the add_raster method of ggbrain_layer, we use 'new_val' as the column name to plot. Create a glue expression here
          # that will be evaluated at the time of the add_raster.
          fill_expr <- rlang::as_label(value$outline)
          if (fill_expr != private$pvt_fill_column) {
            private$pvt_fill_glue <- sub(private$pvt_fill_column, "{new_val}", fill_expr, fixed = TRUE)
          }

          # always pass through the group as the fill column so that the outline conversion gets the grouping right
          private$pvt_group_column <- private$pvt_fill_column
          private$pvt_has_fill <- TRUE
          if (is.null(private$pvt_size)) private$pvt_size <- 1L
        }

        # allow for additional outline splits by another discrete variable
        # useful for when outlines should be colored by one variable (e.g., network), but drawn more finely by another (e.g., region)
        if (!is.null(value$group)) {
          private$pvt_group_column <- c(private$pvt_group_column, rlang::as_name(value$group))
        }
      }
    },

    #' @field outline controls color of outline draw around non-NA (valid) voxels
    #' @details note that the ggbrain_layer_outline class maps onto *_fill fields
    outline = function(value) {
      if (missing(value)) {
        private$pvt_fill
      } else {
        checkmate::assert_string(value)
        private$pvt_fill <- value
        private$pvt_has_fill <- TRUE
        if (is.null(private$pvt_size)) private$pvt_size <- 1L
      }
    },

    #' @field outline_scale a scale_fill_* object containing the ggplot2 outline color scale for this layer
    outline_scale = function(value) {
      if (missing(value)) {
        return(private$pvt_fill_scale)
      } else {
        super$set_scale(value)
      }
    },

    #' @field size controls size of outline drawn around non-NA (valid) voxels
    size = function(value) {
      if (missing(value)) {
        private$pvt_size
      } else {
        checkmate::assert_integerish(value, len=1L, lower=1)
        private$pvt_size <- value
      }
    },
    
    #' @field dil_ero controls the number of pixels to dilate (> 0) or erode (< 0) the outline
    dil_ero = function(value) {
      if (missing(value)) {
        private$pvt_dil_ero
      } else {
        checkmate::assert_integerish(value, len=1L, lower = -1e3, upper = 1e3)
        private$pvt_dil_ero <- as.integer(value)
      }
    }
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param name the name of this layer, used for referencing in layer and panel modifications
    #' @param definition an optional character string defining the image or contrast that should be used
    #'   to lookup data from a ggbrain_slices object. This is mostly used internally by the ggbrain + syntax
    #'   to allow layers to be defined without data in advance of the plot.
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
    #' @param mapping the aesthetic mapping of the layer data to the display. Should be an aes() object and supports
    #'   `outline` (color of outline around clusters). Default is `aes(outline=value)`, which maps the numeric value of the layer data
    #'   to the outline color of the squares at around spatial regions. For labeled data, you might use \code{aes(fill=<label_col_name>)}.
    #' @param outline A character string indicating the color used to outline all non-NA pixels in this layer. This is used in
    #'   distinction to \code{mapping=aes(outline=<variable>)}.
    #' @param outline_scale a ggplot scale object used for mapping the value column as the outline color for the layer.
    #' @param size controls the thickness of outlines
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
    #'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
    #' @param fill_holes the size of holes (in pixels) inside clusters to be filled by nearest neighbor imputation prior to display
    #' @param remove_specks the size of specks (in pixels) to be removed from each slice prior to display
    #' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel
    #' @param dil_ero the number of pixels to dilate (> 0) or erode (<0) the outline.
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns

    initialize = function(name = NULL, definition = NULL, limits = NULL, breaks = integer_breaks(),
      show_legend = TRUE, interpolate = NULL, unify_scales = TRUE, alpha = NULL, mapping = ggplot2::aes(outline = NULL, fill=NULL),
      outline = NULL, outline_scale = NULL, size = NULL, blur_edge=NULL, fill_holes = NULL, remove_specks = NULL,
      trim_threads = NULL, dil_ero = NULL, data = NULL) {

      # common initialization steps
      super$initialize(
        name, definition, limits, breaks, show_legend, interpolate, unify_scales,
        alpha, blur_edge, fill_holes, remove_specks, trim_threads, data
      )

      # outline-specific initialization
      if (!is.null(outline)) self$outline <- outline # fixed outline
      self$outline_scale <- outline_scale

      if (!is.null(mapping)) self$mapping <- mapping # aesthetic mapping
      if (!is.null(size)) self$size <- size
      
      if (!is.null(dil_ero)) self$dil_ero <- dil_ero
      
      # set default color of fixed outline if none provided
      if (isFALSE(private$pvt_has_fill) && is.null(self$mapping$outline)) {
        message("No outline color or mapping set for geom_outline. Defaulting to cyan outline color.")
        self$outline <- "cyan"
      }
    }
  )

)
