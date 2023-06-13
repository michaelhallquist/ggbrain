#' R6 class for a single layer of a ggbrain panel
#' @details 
#'   Note that this class is exported only for power users and rarely needs to be called directly
#'     in typical use of the package. Instead, look at geom_brain() and geom_outline().
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes aes_string geom_raster
#'   guide_colorbar
#' @importFrom ggnewscale new_scale_fill
#' @importFrom Matrix sparseMatrix
#' @importFrom imager as.cimg as.pixset pixset split_connected
#' @importFrom rlang sym
#' @importFrom dplyr group_by filter row_number select
#' @return a `ggbrain_layer` R6 class containing fields related to a visual layer on the `ggbrain` plot
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_name = NULL,
    pvt_definition = NULL,
    pvt_source = NULL, # character string specifying which layer (image/contrast) within ggbrain_slices pertains
    pvt_data = NULL, # the data.frame containing values for this layer
    pvt_mapping = NULL, # how to map the data columns to the display

    pvt_fill_expr = "{new_val}", # expression for aes(fill=) that modifies pvt_fill_column, such as aes(fill=factor(value))
    pvt_fill_column = NULL, # the column within pvt_data containing the values to be mapped to the fill aesthetic in geom_raster
    pvt_fill_scale = NULL, # a ggplot2 scale_fill* object for the layer fill
    pvt_fill = NULL, # the fixed color (string) of the color to use on the fill layer -- setting, not mapping
    pvt_has_fill = FALSE, # whether this layer has a fill geom
    pvt_map_fill = TRUE, # whether the fill is mapped to a variable or fixed to a single color
    pvt_categorical_fill = FALSE, # if mapped, whether the mapping is categorical
    pvt_alpha_column = NULL, # the column within pvt_data containing values to map to alpha aesthetic in geom_raster
    pvt_alpha = NULL, # the fixed alpha transparency for this layer

    pvt_unify_scales = NULL, # whether to equate scale limits or level across panels
    pvt_show_legend = NULL,
    pvt_interpolate = FALSE,
    pvt_is_empty = NULL,
    pvt_all_na = FALSE, # whether the data for this layer are all NA (in which case there's nothing to add)
    pvt_bisided = FALSE, # only set by fill_scale active binding
    pvt_blur_edge = NULL,
    pvt_trim_threads = 0L, # how many neighboring pixels are required to keep this pixel
    pvt_fill_holes = 0L, # how many pixels in a hole within a cluster should be filled by imputation
    pvt_remove_specks = 0L, # how many pixels should constitute a speck to be removed

    # private function to fill holes of a certain size
    # uses a flood fill algorithm from the corners to find pixels that cannot be reached
    # these are then filled by nearest neighbor imputation
    fill_img_holes = function(img, size = 10, neighbors=10) {
      filled_img <- fill_from_edge(img, nedges = 4) # flood fills TRUE from four corners to find holes
      holes <- !as.cimg(filled_img) # becomes TRUE where there is a hole

      # use a component labeler to find holes (assuming there are any)
      if (sum(holes) > 0L) {
        objs <- imager::split_connected(holes) # split into a list of connected components
        obj_size <- sapply(objs, sum) # how many pixels in each
        hh <- which(obj_size <= size) # which holes are small enough to be filled

        if (length(hh) > 0L) {
          # use logical or to combine all holes, assign NA for all hole pixels
          img[as.matrix(Reduce("|", objs[hh]))] <- NA

          # fill NAs by nearest neighbor imputation
          rr <- max(6, ceiling(sqrt(size))) # approximation of radius within which to interpolate
          aggfun <- ifelse(isTRUE(private$pvt_categorical_fill), "mode", "mean")
          img <- nn_impute(img, neighbors = neighbors, radius = rr, aggfun = aggfun, ignore_zeros = TRUE)
        }
      }
      return(img)
    },

    # private function to find specks less than a given size
    find_specks = function(slc, size=NULL, by_roi=FALSE) {
      if (isTRUE(by_roi)) {
        uvals <- unique(as.vector(slc))
        uvals <- uvals[uvals != 0] # 0 is not an ROI label
        if (length(uvals) == 0L) {
          empty_mat <- matrix(FALSE, nrow = nrow(slc), ncol = ncol(slc))
          return(empty_mat) # don't attempt to remove specks on an empty image -- return no specks/all FALSE
        }

        slist <- lapply(uvals, function(u) {
          m <- matrix(0, nrow = nrow(slc), ncol = ncol(slc))
          m[slc == u] <- u
          return(m)
        })
      } else {
        slist <- list(slc) # don't divide speck counting by roi
      }

      # loop over slice (perhaps by ROI) and remove specks
      speck_img <- lapply(slist, function(ss) {
        slc_cimg <- imager::as.cimg(ss)
        all_pix <- imager::as.pixset(slc_cimg) # TRUE for any non-zero pixel
        ret_pix <- imager::pixset(array(FALSE, dim = c(nrow(ss), ncol(ss), 1, 1))) # default pass

        # use a component labeler to find specks (most have at least some pixels to work with)
        if (sum(all_pix) > 0L) {
          objs <- imager::split_connected(all_pix) # split into a list of connected components
          obj_size <- sapply(objs, sum) # how many pixels in each
          specks <- which(obj_size <= size)
          if (length(specks) > 0L) {
            # create a pixset with TRUE for all speck pixels to be dropped
            ret_pix <- Reduce("|", objs[specks])
          }
        }
        return(ret_pix)
      })

      # logical or by ROI to form single pixset of specks
      speck_img <- as.matrix(Reduce("|", speck_img))

      #speck_pos <- which(speck_img == TRUE, arr.ind = TRUE)
      #colnames(speck_pos) <- c("dim1", "dim2")

      return(speck_img) # return logical matrix with positions of specs
    },

    # modify layer data to remove specks, fill holes, and trim threads
    refine_image = function() {
      d <- private$pvt_data
      label_columns <- attr(d, "label_columns")
      dmat <- df2mat(d, replace_na = 0) # convert NAs to zero prior to image processing
      na_rows <- rep(FALSE, nrow(d))
      vcols <- grep("dim1|dim2", names(d), value = TRUE, invert = TRUE)

      # always fill in holes first so that neighbor counts are more sensible
      if (private$pvt_fill_holes > 0L) {
        dmat <- private$fill_img_holes(dmat, size = private$pvt_fill_holes, neighbors = 10)
        d <- mat2df(dmat, na_zeros = TRUE) # convert back to modified data frame
        attr(d, "label_columns") <- label_columns

        # need to fill any ancillary columns with imputed values, too!
        if (!is.null(label_columns)) {
          lab_df <- private$pvt_data %>%
            dplyr::group_by(value) %>%
            dplyr::filter(dplyr::row_number() == 1L & !is.na(value)) %>%
            dplyr::select(value, !!label_columns)

          d <- d %>% left_join(lab_df, by="value")
        }
      }

      # dim1.dim2 string used for matching rows to drop/NA
      dind <- paste(d$dim1, d$dim2, sep = ".")

      # find any thread pixels (iteratively find pixels with few neighbors)
      if (private$pvt_trim_threads > 0L) {
        threads <- find_threads(dmat, min_neighbors=private$pvt_trim_threads, maxit=20L, diagonal=TRUE)
        if (any(threads)) {
          dmat[threads] <- 0 # set threads to 0 in matrix form
          # convert to single row.col match string
          to_trim <- apply(which(threads, arr.ind = TRUE), 1, paste, collapse = ".")
          na_rows <- na_rows | (dind %in% to_trim)
        }
      }

      # remove any clusters with fewer than this number of pixels
      if (private$pvt_remove_specks > 0L) {
        # when we have a categorical fill mapping, we need to use the right column/label for finding specks. Otherwise,
        # "value" is used, which may be a lower-level variable (e.g., ROI) compared to the fill column (e.g., network),
        # leading to unintended removal of 'specks' from the lower-level variable.
        if (isTRUE(private$pvt_categorical_fill) && private$pvt_fill_column != "value") {
          # create a data.frame where the value column contains the integer values of the correct fill variable
          d_tmp <- d %>%
            dplyr::select(-value) %>%
            dplyr::rename(value=!!private$pvt_fill_column) %>%
            dplyr::mutate(value=as.integer(as.factor(value)))
          to_process <- df2mat(d_tmp, replace_na = 0)
        } else {
          to_process <- dmat
        }

        specks <- private$find_specks(to_process, size = private$pvt_remove_specks, by_roi = private$pvt_categorical_fill)
        if (any(specks)) {
          dmat[specks] <- 0 # probably a waste of CPU time -- dmat isn't used again
          # convert to single row.col match string
          to_trim <- apply(which(specks, arr.ind = TRUE), 1, paste, collapse = ".")
          na_rows <- na_rows | (dind %in% to_trim)
        }
      }

      # set any trimmed voxels to NA
      if (any(na_rows)) d[na_rows, vcols] <- NA

      # refresh dataset with refined values
      private$pvt_data <- d
    },

    # helper function for adding a mapped geom_raster to an existing ggplot
    add_raster = function(gg, df, value_col = NULL, n_layers, raster_args = NULL, fill_scale = NULL) {
      checkmate::assert_data_frame(df, null.ok = TRUE)
      if (is.null(df) || nrow(df) == 0L) {
        return(gg)
      } # no change

      # When alpha is < 1, na.value="transparent" doesn't work as expected. Need to use na.omit()
      if (!is.null(private$pvt_alpha_column) || (!is.null(private$pvt_alpha) && private$pvt_alpha < 1)) {
        df <- df %>% dplyr::filter(!is.na(!!rlang::sym(value_col)))
      }

      if (!is.null(fill_scale)) {
        # mapped fill layer -- for the multiple fill scale approach to work, column names need to be unique across layers (value1, value2, etc.)
        new_val <- paste0("value", n_layers + 1L)
        df <- df %>% dplyr::rename(!!new_val := !!value_col)
        if (n_layers > 0L) {
          # if there is already a layer, we need to create a new slot for a fill aesthetic
          gg <- gg + ggnewscale::new_scale_fill()
        }

        # evaluate fill expression and pass forward to aes as character
        fill_expr <- sub("{new_val}", new_val, private$pvt_fill_expr, fixed=TRUE)

        raster_args$data <- df
        raster_args$mapping <- ggplot2::aes_string(x = "dim1", y = "dim2", fill = fill_expr, alpha = private$pvt_alpha_column)
        # Based on weird ggplot2 + ggnewscale bug, only set show.legend when it is FALSE to avoid legend collisions
        # https://github.com/eliocamp/ggnewscale/issues/32
        if (isFALSE(private$pvt_show_legend)) raster_args$show.legend <- private$pvt_show_legend
      } else {
        # fixed fill layer -- always need to drop NAs from data because when fill is *set* (not mapped), any row in the
        # data.frame will be filled with the specified color.
        raster_args$data <- subset(df, !is.na(value))
        raster_args$mapping <- ggplot2::aes_string(x = "dim1", y = "dim2", fill = NULL, alpha = private$pvt_alpha_column)
        raster_args$fill <- private$pvt_fill
        raster_args$show.legend <- FALSE
      }

      robj <- do.call(geom_raster, raster_args)
      # robj <- do.call(geom_tile, raster_args) # for comparison re: warnings about uneven intervals
      gg <- gg + robj + fill_scale

      return(gg)
    },

    # general private method for returning data to plot on slice
    # will be overloaded by subclasses for specific purposes
    get_plot_data = function() {

      if (isFALSE(private$pvt_has_fill)) {
        return(NULL) # no fill data
      }

      if (isTRUE(private$pvt_map_fill)) {
        vcol <- private$pvt_fill_column # which column in the dataset has the value to map onto the raster
      } else {
        vcol <- "value" # we have a fill layer, but it is not mapped -- this occurs for outline layers built from continuous images
      }

      # Handle bisided situation
      # Note that geom_raster does best when there is a value at every square. Otherwise, it throws:
      # "Raster pixels are placed at uneven vertical intervals and will be shifted"
      # This appears to be ignorable, but is concerning and slightly annoying as a warning message
      # Thus, we create parallel data.frames with NAs, rather than a simple filter for pos or neg
      if (isTRUE(private$pvt_bisided) && isFALSE(private$pvt_categorical_fill)) {
        df <- NULL
        df_neg <- private$pvt_data %>%
          dplyr::mutate(!!vcol := if_else(!!sym(vcol) < 0, !!sym(vcol), NA_real_))

        df_pos <- private$pvt_data %>%
          dplyr::mutate(!!vcol := if_else(!!sym(vcol) > 0, !!sym(vcol), NA_real_))

        private$symmetrize_limits(df, df_pos)

        fill_scale <- NULL
        fill_scale_neg <- private$pvt_fill_scale$neg_scale
        fill_scale_pos <- private$pvt_fill_scale$pos_scale
      } else {
        df <- private$pvt_data
        df_pos <- df_neg <- NULL
        fill_scale <- private$pvt_fill_scale
        fill_scale_pos <- fill_scale_neg <- NULL
      }

      bisided <- private$pvt_bisided

      return(named_list(df, df_pos, df_neg, fill_scale, fill_scale_pos, fill_scale_neg, vcol, bisided))

    },

    set_scale = function(value) {
      if (is.null(value)) {
        private$pvt_fill_scale <- NULL # reset
      } else if (checkmate::test_class(value, "Scale")) {
        stopifnot(value$aesthetics == "fill")

        # make sure that NAs are always drawn as transparent
        # if na.value is NA, omit it so that NA is not shown as a factor level
        if (!is.na(value$na.value) && value$na.value != "transparent") {
          value$na.value <- "transparent"
        }

        # default color scale to name of layer if not otherwise provided
        if (is.null(value$name) || inherits(value$name, "waiver")) {
          value$name <- self$name
        }

        if (inherits(value, "ScaleBisided")) {
          private$pvt_bisided <- TRUE # flag this layer as bisided

          # propagate na.value to pos and neg components
          value$pos_scale$na.value <- value$na.value
          value$neg_scale$na.value <- value$na.value
          value$pos_scale$name <- value$name
          value$neg_scale$name <- "" # no label on negative scale
        } else {
          private$pvt_bisided <- FALSE
        }

        # preserve breaks if $set_breaks() is called before $set_scale()
        if (!is.null(private$pvt_fill_scale) && !checkmate::test_class(private$pvt_fill_scale, "Scale")) {
          cached_breaks <- private$pvt_fill_scale$breaks
        } else {
          cached_breaks <- NULL
        }

        private$pvt_fill_scale <- value

        if (!is.null(cached_breaks)) {
          if (isTRUE(private$pvt_bisided)) {
            self$set_pos_breaks(cached_breaks)
            self$set_neg_breaks(cached_breaks)
          } else {
            self$set_breaks(cached_breaks)
          }
        }
      } else {
        stop("Cannot understand scale input. Should be a scale_fill_* object.")
      }
    },

    # finalizing function that inspects data just before attempting to plot it
    validate_layer = function() {
      if (self$is_empty()) return(invisible(NULL)) # nothing to do on empty object

      private$pvt_has_fill <- FALSE
      if (!is.null(private$pvt_fill_column)) {
        if (!private$pvt_fill_column %in% names(private$pvt_data)) {
          stop(glue::glue("Cannot find specified fill column {private$pvt_fill_column} in layer data"))
        } else {
          private$pvt_has_fill <- TRUE # we have a fill geom
          private$pvt_map_fill <- TRUE # map fill color to data

          fcol_cat <- inherits(private$pvt_data[[private$pvt_fill_column]], c("character", "factor", "ordered"))

          # if layer does not already believe that fill is categorical column, but the fill column in the data is categorical, set to TRUE
          if (!isTRUE(private$pvt_categorical_fill)) {
            # if fill column is character or factor, then fill is categorical
            private$pvt_categorical_fill <- fcol_cat
          } else if (!fcol_cat) {
            # Fill is supposed to be categorical, but column is not a categorical data type. Convert to factor
            private$pvt_data[[private$pvt_fill_column]] <- factor(private$pvt_data[[private$pvt_fill_column]])
          }
        }
      }

      # check for fixed fill color in layer -- this will supersede mapping check in pvt_fill_column TRUE above
      if (!is.null(private$pvt_fill)) {
          private$pvt_has_fill <- TRUE # we have a fill geom
          private$pvt_map_fill <- FALSE # fixed fill color
          private$pvt_categorical_fill <- FALSE # just a fixed value, not a mapped categorical variable
      }

      # ensure that alpha is set to NA if we have an alpha mapping
      if (!is.null(private$pvt_alpha_column)) {
        private$pvt_alpha <- NULL
      }

    },

    # helper function to make positive and negative scales symmetric
    symmetrize_limits = function(df_neg, df_pos) {
      # if positive and negative limits are already set, then respect these
      # (most commonly, this is due to unify_scales)

      # don't symmetrize if not requested
      if (!private$pvt_fill_scale$symmetric) return(invisible(NULL))

      neg_lims <- private$pvt_fill_scale$neg_scale$limits
      pos_lims <- private$pvt_fill_scale$pos_scale$limits

      if (is.null(neg_lims)) {
        neg_lims <- c(min(df_neg$value, na.rm = TRUE), max(df_neg$value, na.rm = TRUE))
      }

      if (is.null(pos_lims)) {
        pos_lims <- c(min(df_pos$value, na.rm = TRUE), max(df_pos$value, na.rm = TRUE))
      }

      biggest <- max(abs(neg_lims[1]), pos_lims[2], na.rm=TRUE)
      smallest <- min(abs(neg_lims[2]), pos_lims[1], na.rm=TRUE)

      private$pvt_fill_scale$neg_scale$limits <- -1 * c(biggest, smallest)
      private$pvt_fill_scale$pos_scale$limits <- c(smallest, biggest)
    },

    # private method to set default fill scale when not provided
    set_default_scale = function() {
      # don't set default if no data exist or if an existing scale is present
      if (!is.null(private$pvt_fill_scale) || is.null(private$pvt_data)) {
        return(invisible(NULL)) # don't modify extant scale
      }

      # detect appropriate default scale
      if (private$pvt_definition == "underlay") {
        self$fill_scale <- scale_fill_gradient(low = "grey8", high = "grey92")
        self$show_legend <- FALSE # default to hiding underlay scale
      } else {
        has_pos <- any(private$pvt_data$value > 0, na.rm = TRUE)
        has_neg <- any(private$pvt_data$value < 0, na.rm = TRUE)
        all_na <- all(is.na(private$pvt_data$value))
        if (isTRUE(private$pvt_categorical_fill)) {
          self$fill_scale <- scale_fill_brewer(palette = "Set3")
        } else if (has_pos && has_neg) {
          #self$fill_scale <- scale_fill_distiller(palette = "RdBu") # red-blue diverging
          self$fill_scale <- scale_fill_bisided(
            neg_scale = scale_fill_distiller(palette = "Blues", direction = 1),
            pos_scale = scale_fill_distiller(palette = "Reds")
          ) # internal scale stack
        } else if (has_neg) {
          self$fill_scale <- scale_fill_distiller(palette = "Blues", direction = 1)
        } else if (has_pos) {
          self$fill_scale <- scale_fill_distiller(palette = "Reds")
        } else if (all_na) {
          self$fill_scale <- scale_fill_distiller(palette = "Reds")
        } else {
          warning("Could not set default scale for layer")
        }
        if (is.null(self$show_legend)) self$show_legend <- TRUE
      }
      return(invisible(NULL))
    }
  ),
  active = list(
    #' @field name the name of this layer, used for referencing in layer and panel modifications
    name = function(value) {
      if (missing(value)) {
        return(private$pvt_name)
      } else {
        checkmate::assert_string(value)
        private$pvt_name <- value
      }
    },

    #' @field all_na whether all values for this layer are NA in the \code{data} field
    all_na = function(value) {
      if (missing(value)) {
        return(private$pvt_all_na)
      } else {
        stop("Cannot directly set the all_na field. This is set via setting the $data field.")
      }
    },

    #' @field definition a character string specifying the image name or contrast that defines this layer
    definition = function(value) {
      if (missing(value)) {
        return(private$pvt_definition)
      } else {
        checkmate::assert_string(value)
        private$pvt_definition <- value
      }
    },

    #' @field source a character string specifying the layer source within a relevant ggbrain_slices object.
    #'   This is used to lookup the right layer information when combining slices and layers together
    #'   Note that multiple layers can potentially have the same source, which is why a 1:1 mapping to name does not work
    source = function(value) {
      if (missing(value)) {
        return(private$pvt_source)
      } else {
        checkmate::assert_string(value)
        private$pvt_source <- value
      }
    },

    #' @field data the data.frame containing relevant data for this layer.
    data = function(value) {
      if (missing(value)) {
        return(private$pvt_data)
      } else {
        if (is.null(value)) {
          private$pvt_data <- NULL # clear data
          private$pvt_is_empty <- TRUE
        } else {
          checkmate::assert_data_frame(value)
          if (!all(c("dim1", "dim2", "value") %in% names(value))) {
            stop("data must contain dim1, dim2, and value")
          }

          # check whether all data for this layer are NA
          if (all(is.na(value$value))) {
            private$pvt_all_na <- TRUE
          } else {
            private$pvt_all_na <- FALSE
          }

          private$pvt_data <- value
          private$pvt_is_empty <- FALSE # always make is_empty FALSE when we have data
          private$validate_layer() # always validate the layer based on the data (identifies the form of fill mapping)
          private$set_default_scale() # add default scale, if needed, after modifying data
          private$refine_image() # trim threads, remove specks, fill holes

          # ensure that numeric breaks are not used with a categorical scale
          # (note that this doesn't allow custom breaks in categorical layers yet... so, it's a hack)
          if (isTRUE(private$pvt_categorical_fill)) {
            private$pvt_fill_scale$breaks <- ggplot2::waiver()
          }
        }
      }
    },

    #' @field show_legend a logical indicating whether to show or hide the fill/color scale
    show_legend = function(value) {
      if (missing(value)) {
        return(private$pvt_show_legend)
      } else {
        checkmate::assert_logical(value, len = 1L)
        private$pvt_show_legend <- value
      }
    },

    #' @field unify_scales a logical indicating whether to unify scale limits and levels when this layer
    #'   is added across many panels
    unify_scales = function(value) {
      if (missing(value)) {
        private$pvt_unify_scales
      } else {
        checkmate::assert_logical(value, len=1L)
        private$pvt_unify_scales <- value
      }
    },

    #' @field bisided read-only access to whether this layer uses a bisided color scale
    bisided = function(value) {
      if (missing(value)) private$pvt_bisided
      else stop("Cannot assign bisided")
    },

    #' @field categorical_fill read-only access to whether this layer has a categorical fill scale
    categorical_fill = function(value) {
      if (missing(value)) private$pvt_categorical_fill
      else stop("Cannot set categorical_fill field")
    },

    #' @field fill_column read-only access to layer fill column
    fill_column = function(value) {
      if (missing(value)) private$pvt_fill_column
      else stop("Cannot set fill_column field")
    },

    #' @field fill_scale a scale_fill_* object containing the ggplot2 fill scale for this layer
    fill_scale = function(value) {
      if (missing(value)) {
        return(private$pvt_fill_scale)
      } else {
        private$set_scale(value)
      }
    },

    #' @field alpha sets the alpha transparency of this layer.
    alpha = function(value) {
      if (missing(value)) {
        return(private$pvt_alpha)
      } else {
        checkmate::assert_number(value, lower = 0, upper = 1)
        private$pvt_alpha <- value
      }
    },

    #' @field blur_edge controls the standard deviation (sigma) of a Gaussian blur applied to the layer at the edge
    blur_edge = function(value) {
      if (missing(value)) {
        return(private$pvt_blur_edge)
      } else {
        checkmate::assert_number(value, lower = 0)
        private$pvt_blur_edge <- value
      }
    },

     #' @field trim_threads iteratively trim any pixels that have fewer than this number of neighboring pixels
    trim_threads = function(value) {
      # if user passes in TRUE/FALSE, then use default of 3 neighbors
      if (checkmate::test_logical(value)) {
        stopifnot(length(value) == 1L)
        private$pvt_trim_threads <- 3L # default to trimming pixels with 3 or fewer neighbors (incl. diagonal)
      } else if (checkmate::test_integerish(value)) {
        value <- as.integer(value)
        checkmate::assert_integer(value, lower = 1L, upper=8L)
        private$pvt_trim_threads <- value
      } else {
        stop("Cannot interpret trim_threads")
      }
    },

    #' @field fill_holes controls the size of holes to be filled for display (in pixels)
    fill_holes = function(value) {
      # if user passes in TRUE/FALSE, then use default size of 10 pixels
      if (checkmate::test_logical(value)) {
        stopifnot(length(value) == 1L)
        private$pvt_fill_holes <- 10L # default to 10 pixels or smaller
      } else if (checkmate::test_integerish(value)) {
        value <- as.integer(value)
        checkmate::assert_integer(value, lower = 0L)
        private$pvt_fill_holes <- value
      } else {
        stop("Cannot interpret fill_holes")
      }
    },

    #' @field remove_specks controls the size of specks to be removed (in pixels)
    remove_specks = function(value) {
      # if user passes in TRUE/FALSE, then use default size of 10 pixels
      if (checkmate::test_logical(value)) {
        stopifnot(length(value) == 1L)
        private$pvt_remove_specks <- 10L # default to 10 pixels or smaller
      } else if (checkmate::test_integerish(value)) {
        value <- as.integer(value)
        checkmate::assert_integer(value, lower = 0L)
        private$pvt_remove_specks <- value
      } else {
        stop("Cannot interpret remove_specks")
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
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param alpha fixed alpha transparency of this layer (use `mapping` for alpha mapping`)
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
    #'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
    #' @param fill_holes the size of holes (in pixels) inside clusters to be filled by nearest neighbor imputation prior to display
    #' @param remove_specks the size of specks (in pixels) to be removed from each slice prior to display
    #' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    initialize = function(name = NULL, definition = NULL, limits = NULL, breaks = integer_breaks(),
      show_legend = TRUE, interpolate = NULL, unify_scales=TRUE, alpha = NULL, blur_edge = NULL,
      fill_holes = NULL, remove_specks = NULL, trim_threads = NULL, data = NULL) {

      # if name is NULL, see if layer is named through the definition field using <name> := <value>
      if (is.null(name)) {
        # look at definition and use the name before := as layer name if available. Otherwise, use "layer".
        name <- contrast_split(definition, no_name = "layer")$name
      }
      checkmate::assert_numeric(limits, len = 2L, null.ok = TRUE)
      checkmate::assert_logical(interpolate, len=1L, null.ok = TRUE)

      # uses active bindings to validate inputs
      self$name <- name
      self$show_legend <- show_legend
      self$definition <- definition
      self$unify_scales <- unify_scales

      # allow for empty layers with data added later
      self$data <- data

      if (!is.null(interpolate)) private$pvt_interpolate <- interpolate
      if (!is.null(limits)) self$set_limits(limits)
      if (!is.null(breaks)) self$set_breaks(breaks)
      if (!is.null(alpha)) self$alpha <- alpha
      if (!is.null(blur_edge)) self$blur_edge <- blur_edge
      if (!is.null(fill_holes)) self$fill_holes <- fill_holes
      if (!is.null(remove_specks)) self$remove_specks <- remove_specks
      if (!is.null(trim_threads)) self$trim_threads <- trim_threads
    },

    #' @description set the limits for this layer's scale
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's scale
    set_limits = function(limits) {
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_fill_scale$limits <- limits
      return(self)
    },

    #' @description set the limits for this layer's positive scale (only relevant to bisided)
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's positive scale
    set_pos_limits = function(limits) {
      stopifnot(isTRUE(private$pvt_bisided))
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_fill_scale$pos_scale$limits <- limits
      return(self)
    },

    #' @description set the limits for this layer's positive scale (only relevant to bisided)
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's positive scale
    set_neg_limits = function(limits) {
      stopifnot(isTRUE(private$pvt_bisided))
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_fill_scale$neg_scale$limits <- limits
      return(self)
    },

    #' @description set the breaks element of this layer's scale
    #' @param breaks a function used to label the breaks
    set_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_fill_scale$breaks <- breaks
    },

    #' @description set the breaks element of this layer's positive scale (only relevant to bisided)
    #' @param breaks a function used to label the positive breaks
    set_pos_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_fill_scale$pos_scale$breaks <- breaks
    },

    #' @description set the breaks element of this layer's negative scale (only relevant to bisided)
    #' @param breaks a function used to label the negative breaks
    set_neg_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_fill_scale$neg_scale$breaks <- breaks
    },

    #' @description plot this layer alone (mostly for debugging)
    plot = function() {
      if (self$is_empty()) stop("Cannot generate plot because this layer has no data! Use $data to add.")

      # create empty plot
      g <- ggplot(data = private$pvt_data, aes(x=dim1, y=dim2, fill=value))

      # add relevant marks to layer
      g <- self$add_to_gg(g)

      return(g)
    },

    #' @description method to add this layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      # skip out if no data exist in this layer
      if (self$is_empty()) {
        warning(glue::glue("No data in layer {self$name}! Not adding to ggplot object."))
        return(base_gg)
      }

      # Oct2022: disabling this because it will throw an error if the only layer is an all-NA layer
      # but I added it a while ago to handle a different problem that I can't entirely recall... I think I
      # refactored the code properly to handke NAs in layers, however.
      # } else if (isTRUE(self$all_na)) {
      #   return(base_gg) # nothing to add if the data have no non-NA values
      # }

      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      n_scales <- length(base_gg$scales$scales) # will be less than n_layers when show.legend is FALSE
      ret <- base_gg # ggplot object to modify and return

      # if there is no fill data (mapped or set), there is nothing to add to the ggplot object
      if (!private$pvt_has_fill) return(base_gg)

      # obtain data to plot
      pdata <- private$get_plot_data()
      if (isTRUE(pdata$bisided)) {
        df <- pdata$df_neg
        df_pos <- pdata$df_pos
        fill_scale <- pdata$fill_scale_neg
        fill_scale_pos <- pdata$fill_scale_pos
      } else {
        df <- pdata$df
        df_pos <- NULL
        fill_scale <- pdata$fill_scale
        fill_scale_pos <- NULL
      }

      raster_args <- list(interpolate = private$pvt_interpolate)
      if (!is.null(private$pvt_alpha)) raster_args$alpha <- private$pvt_alpha # adding NULL or NA messes up alpha

      has_df <- ifelse(nrow(df) > 0L, TRUE, FALSE)
      has_pos_df <- ifelse(!is.null(df_pos) && nrow(df_pos) > 0L, TRUE, FALSE)

      # only add fills if a fill setting/mapping exists
      if (isTRUE(private$pvt_map_fill)) {
        if (has_df) {
          ret <- private$add_raster(ret, df, pdata$vcol, n_layers, raster_args, fill_scale)
          n_layers <- n_layers + 1
        }

        if (isTRUE(private$pvt_bisided) && isTRUE(has_pos_df)) {
          ret <- private$add_raster(ret, df_pos, pdata$vcol, n_layers, raster_args, fill_scale_pos)
          n_layers <- n_layers + 1

          # if bisided has both positive and negative layers/data, we need to control color bar order
          if (isTRUE(private$pvt_show_legend) && has_df && has_pos_df) {
            # force color bar order: +1 is negative, +2 is positive. Lower orders are positioned lower on legend
            ret$scales$scales[[n_scales + 1]]$guide <- guide_colorbar(order = n_scales + 2, available_aes = c("fill", "fill_new"))
            ret$scales$scales[[n_scales + 2]]$guide <- guide_colorbar(order = n_scales + 1, available_aes = c("fill", "fill_new"))
          }
        }
      } else {
        # fixed fill color (drop scale)
        ret <- private$add_raster(ret, df, pdata$vcol, n_layers, raster_args, fill_scale = NULL)
      }

      return(ret)
    },

    #' @description return the data.frame associated with this layer
    #' @param add_layer_name if TRUE, adds a \code{layer_name} column to the data.frame for record-keeping.
    #'   Default: FALSE.
    get_data = function(add_layer_name = FALSE) {
      if (isTRUE(add_layer_name)) {
        df <- private$pvt_data
        df$layer_name <- private$pvt_name
        return(df)
      } else {
        return(private$pvt_data)
      }
    },

    #' @description returns TRUE if all values are NA or if the data has 0 rows
    is_empty = function() {
      private$pvt_is_empty
    }
  )
)

# cf. https://stackoverflow.com/questions/67279921/how-to-use-ggplot-add-inside-another-package

#' S3 method to support adding ggbrain_layer objects to an existing ggplot object
#' @param object the ggbrain_layer object to be added to an existing ggplot
#' @param plot the ggplot object
#' @param object_name not used, but required by ggplot_add
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_layer <- function(object, plot, object_name) {
  object$add_to_gg(plot) # adds the layer to the extant plot
}
