#' R6 class for a single layer of a ggbrain panel using outline geom
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes
#' @importFrom Matrix sparseMatrix
#' @importFrom imager as.cimg erode_square isoblur boundary
#' @export
ggbrain_layer_outline <- R6::R6Class(
  classname = "ggbrain_layer_outline",
  inherit = ggbrain_layer,
  private = list(
    pvt_outline_size = NULL,
    pvt_group_column = NULL, # handles case where we want to control how outlines are defined/grouped (e.g., by region versus by label)

    get_plot_data = function() {
      if (is.null(self$outline_size)) return(NULL) # if outline_size is NULL, there is nothing to plot

      data_list <- super$get_plot_data() # general data

      # convert all relevant data.frames to outlines
      data_list[c("df", "df_pos", "df_neg")] <- lapply(
        data_list[c("df", "df_pos", "df_neg")], private$slice_to_outline, private$pvt_group_column, private$pvt_blur_edge
      )

      return(data_list)
    },

    # function that traces the outline of the slice, grouped by image
    slice_to_outline = function(df, group_col = NULL, blur_sigma = 0.9) {
      if (is.null(df)) return(NULL) # don't attempt to outline an empty object

      # always drop NAs before proceeding with outline since only non-NA points can contribute
      max_dim1 <- max(df$dim1) # extent of x axis (before na.omit)
      max_dim2 <- max(df$dim2)
      df <- df %>% na.omit()

      # if we have a factor, split on this and get outlines for each component
      if (!is.null(group_col)) {
        stopifnot(group_col %in% names(df))
        df_split <- df %>% group_split(across(all_of(group_col)), .keep = TRUE)
        by_roi <- TRUE
      } else {
        df_split <- list(df)
        by_roi <- FALSE
      }

      df_melt <- bind_rows(lapply(df_split, function(dd) {
        # convert to a 0/1 matrix where 1s denote present voxels
        slc_mat <- as.matrix(sparseMatrix(i = dd$dim1, j = dd$dim2, x = 1, dims = c(max_dim1, max_dim2)))
        slc_img <- imager::as.cimg(slc_mat) # convert to cimg object

        # follows this logic: https://stackoverflow.com/questions/29878844/how-do-i-find-the-perimeter-of-objects-in-a-binary-image
        # er <- imager::erode_square(slc_img, 3) # erode by very small square
        # res1 <- abs(er - slc_img) # retained images become -1 -- not sure why this is needed...
        # res <- slc_img - er # retained images become -1

        # simpler approach of using imager boundary function
        b <- imager::boundary(slc_img, depth = private$pvt_outline_size)
        if (!is.null(blur_sigma)) {
          orig <- as.cimg(b)
          res <- imager::isoblur(orig, sigma = blur_sigma)
          res[b] <- 1 # always keep original boundary as alpha 1.0 (don't blur in 0 pixels)
        } else {
          res <- as.cimg(b) # just convert boundary into 1/0 image
        }

        # scale antialiasing of outline by base alpha transparency requested
        base_alpha <- ifelse(is.null(private$pvt_alpha), 1, private$pvt_alpha)

        # need to protect value in AsIs I() notation to get simple alpha mapping in ggplot2
        # https://www.andrea-rau.com/post/alpha-transparency-in-ggplot2/
        # isoblur generates some tiny values! If alpha is less than 5%, treat it as NA/empty
        ret <- reshape2::melt(as.matrix(res), varnames = c("dim1", "dim2")) %>%
          mutate(
            value = if_else(value < .05, 0, value),
            # rescale_max is obviated by approach of keeping the pixset values at 1.0 in the post-blur image
            #value = !!base_alpha*scales::rescale_max(value), # sets the max to 1.0 -- blur can sometimes have values ~0.96
            value = !!base_alpha*value,
            alpha = I(value)
          )

        if (isTRUE(by_roi)) {
          ret[[group_col]] <- dd[[group_col]][1L] # simplest way to preserve factor levels is straight copy
          ret[[group_col]][ret$value < 1e-6] <- NA # set value==0 pixels to NA
        } else {
          ret <- ret %>%
            mutate(value = if_else(value < 1e-6, NA_integer_, 1L))
        }

        return(ret)
      }))

      # handle alpha layering -- need to convert to an alpha mapping internally to get antialiasing to work
      if (!is.null(blur_sigma)) {
        private$pvt_alpha_column <- "alpha" # set alpha aes mapping
        private$pvt_alpha <- NULL # unset the fixed alpha mapping since it has been blended with an alpha mapping
      }

      return(df_melt %>% na.omit())
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
          private$pvt_has_fill <- FALSE
        } else {
          private$pvt_fill_column <- rlang::as_name(value$outline) # pull out the outline column from aes
          # always pass through the group as the fill column so that the outline conversion gets the grouping right
          private$pvt_group_column <- private$pvt_fill_column
          private$pvt_has_fill <- TRUE
          if (is.null(private$pvt_outline_size)) private$pvt_outline_size <- 1L
        }

        if (!is.null(value$group)) {
          private$pvt_group_column <- rlang::as_name(value$group)
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
        if (is.null(private$pvt_outline_size)) private$pvt_outline_size <- 1L
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

    #' @field controls size of outline drawn around non-NA (valid) voxels
    outline_size = function(value) {
      if (missing(value)) {
        private$pvt_outline_size
      } else {
        checkmate::assert_integerish(value, len=1L, lower=1)
        private$pvt_outline_size <- value
      }
    }
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param name the name of this layer, used for referencing in layer and panel modifications
    #' @param definition an optional character string defining the image or contrast that should be used
    #'   to lookup data from a ggbrain_slices object. This is mostly used internally by the ggbrain + syntax
    #'   to allow layers to be defined without data in advance of the plot.
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
    #' @param mapping the aesthetic mapping of the layer data to the display. Should be an aes() object and supports
    #'   `outline` (color of outline around clusters). Default is `aes(outline=value)`, which maps the numeric value of the layer data
    #'   to the outline color of the squares at around spatial regions. For labeled data, you might use aes(fill=<label_col_name>).
    #' @param outline A character string indicating the color used to outline all non-NA pixels in this layer. This is used in
    #'   distinction to mapping=aes(outline=<variable>).
    #' @param outline_scale a ggplot scale object used for mapping the value column as the outline color for the layer.
    #' @param outline_size controls the thickness of outlines
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to smooth it (to make the visual less jagged)
    initialize = function(name = NULL, definition = NULL, data = NULL,
      limits = NULL, breaks = NULL, show_legend = TRUE, interpolate = NULL, unify_scales = TRUE, alpha = NULL,
      mapping = ggplot2::aes(outline = NULL, fill=NULL), outline = NULL, outline_scale = NULL, outline_size = NULL, blur_edge=NULL) {

      # common initialization steps
      super$initialize(name, definition, data, limits, breaks, show_legend, interpolate, unify_scales, alpha, blur_edge)

      # outline-specific initialization
      if (!is.null(outline)) self$outline <- outline # fixed outline
      self$outline_scale <- outline_scale

      if (!is.null(mapping)) self$mapping <- mapping # aesthetic mapping
      if (!is.null(outline_size)) self$outline_size <- outline_size
    }
  )

)
