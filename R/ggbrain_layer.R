#' R6 class for a single layer of a ggbrain panel
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes aes_string
#' @importFrom ggnewscale new_scale_fill
#' @importFrom Matrix sparseMatrix
#' @importFrom imager as.cimg erode_square
#' @importFrom rlang sym
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_name = NULL,
    pvt_definition = NULL,
    pvt_source = NULL, # character string specifying which layer (image/contrast) within ggbrain_slices pertains
    pvt_data = NULL, # the data.frame containing values for this layer
    pvt_mapping = NULL, # how to map the data columns to the display

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
    pvt_bisided = FALSE, # only set by fill_scale active binding
    pvt_blur_edge = NULL,

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
        # mapped fill layer
        new_val <- paste0("value", n_layers + 1L)
        df <- df %>% dplyr::rename(!!new_val := !!value_col)
        if (n_layers > 0L) {
          # if there is already a layer, we need to create a new slot for a fill aesthetic
          gg <- gg + ggnewscale::new_scale_fill()
        }

        raster_args$data <- df
        raster_args$mapping <- ggplot2::aes_string(x = "dim1", y = "dim2", fill = new_val, alpha = private$pvt_alpha_column)
        raster_args$show.legend <- private$pvt_show_legend
      } else {
        # fixed fill layer
        raster_args$data <- df
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
      vcol <- private$pvt_fill_column # which column in the dataset has the value to map onto the raster

      if (isFALSE(private$pvt_has_fill)) {
        return(NULL) # no fill data
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

          # if fill column is character or factor, then fill is categorical
          private$pvt_categorical_fill <- ifelse(checkmate::test_multi_class(private$pvt_data[[private$pvt_fill_column]], c("character", "factor")), TRUE, FALSE)
        }
      } else if (!is.null(private$pvt_fill)) {
          private$pvt_has_fill <- TRUE # we have a fill geom
          private$pvt_map_fill <- FALSE # fixed fill color
          private$pvt_categorical_fill <- FALSE # just a fixed value, not a mapped categorical variable
      }

      # ensure that alpha is set to NA if we have an alpha mapping
      if (!is.null(private$pvt_alpha_column)) {
        private$pvt_alpha <- NULL
      }

      # ensure that numeric breaks are not used with a categorical scale (note that this doesn't allow custom breaks in categorical layers yet... so, it's a hack)
      if (isTRUE(private$pvt_categorical_fill)) {
        private$pvt_fill_scale$breaks <- ggplot2::waiver()
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

          private$pvt_data <- value
          private$pvt_is_empty <- FALSE # always make is_empty FALSE when we have data
          private$validate_layer() # always validate the layer based on the data (identifies the form of fill mapping)
          private$set_default_scale() # add default scale, if needed, after modifying data
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
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param alpha fixed alpha transparency of this layer (use `mapping` for alpha mapping`)
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to smooth it (to make the visual less jagged)
    initialize = function(name = NULL, definition = NULL, data = NULL, limits = NULL, breaks = integer_breaks(),
      show_legend = TRUE, interpolate = NULL, unify_scales=TRUE, alpha = NULL, blur_edge = NULL) {

      if (is.null(name)) name <- "layer"
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

      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      n_scales <- length(base_gg$scales$scales) # will be less than n_layers when show.legend is FALSE
      ret <- base_gg # ggplot object to modify and return

      # determine fill settings based on data prior to plotting
      private$validate_layer()

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
        # fixed fill color
        ret <- private$add_raster(ret, df, value_col = NULL, n_layers, raster_args, fill_scale = NULL)
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
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_layer <- function(object, plot, object_name) {
  object$add_to_gg(plot) # adds the layer to the extant plot
}
