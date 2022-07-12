#' R6 class for a single layer of a ggbrain panel
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller
#' @importFrom ggnewscale new_scale_fill
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_name = NULL,
    pvt_definition = NULL,
    pvt_data = NULL,
    pvt_color_scale = NULL,
    pvt_show_legend = NULL,
    pvt_interpolate = FALSE,
    pvt_is_empty = NULL,

    # private method to set default fill scale when not provided
    set_default_scale = function() {
      # don't set default if no data exist or if an existing scale is present
      if (!is.null(private$pvt_color_scale) || is.null(private$pvt_data)) {
        return(invisible(NULL)) # don't modify extant scale
      }

      # detect appropriate default scale
      if (private$pvt_name == "underlay") {
        self$color_scale <- scale_fill_gradient(low = "grey8", high = "grey92")
        if (is.null(self$show_legend)) self$show_legend <- FALSE # default to hiding underlay scale
      } else {
        has_pos <- any(private$pvt_data$value > 0, na.rm = TRUE)
        has_neg <- any(private$pvt_data$value < 0, na.rm = TRUE)
        if (has_pos && has_neg) {
          self$color_scale <- scale_fill_distiller(palette = "RdBu") # red-blue diverging
        } else if (has_neg) {
          self$color_scale <- scale_fill_distiller(palette = "Blues", direction = 1)
        } else if (has_pos) {
          self$color_scale <- scale_fill_distiller(palette = "Reds")
        } else {
          stop("Cannot find positive or negative values") # TODO: support discrete/character labels
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

    #' @field data the data.frame containing relevant data for this layer.
    data = function(value) {
      if (missing(value)) {
        return(private$pvt_data)
      } else {
        if (is.null(value)) {
          private$pvt_data <- NULL # clear data
          private$pvt_is_empty <- TRUE
        } else {
          checkmate::assert_data_frame(data)
          if (!all(c("dim1", "dim2", "value") %in% names(data))) {
            stop("data must contain dim1, dim2, and value")
          }

          private$pvt_data <- value
          private$pvt_is_empty <- FALSE # always make is_empty FALSE when we have data
          private$set_default_scale() # add default scale, if needed, after modifying data
        }
      }
    },

    #' @field color_scale a scale_fill_* object containing the ggplot2 fill scale for this layer
    color_scale = function(value) {
      if (missing(value)) {
        return(private$pvt_color_scale)
      } else if (is.null(value)) {
        private$pvt_color_scale <- NULL # reset
      } else if (checkmate::test_class(value, "Scale")) {
        stopifnot(value$aesthetics == "fill")

        # make sure that NAs are always drawn as transparent
        if (value$na.value != "transparent") {
          value$na.value <- "transparent"
        }

        private$pvt_color_scale <- value
      } else {
        stop("Cannot understand color_scale input. Should be a scale_fill_* object.")
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
    #' @param color_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    initialize = function(name = NULL, definition = NULL, data = NULL, color_scale = NULL, limits = NULL,
                          breaks = NULL, show_legend = TRUE, interpolate = NULL) {

      if (is.null(name)) name <- "layer"
      checkmate::assert_numeric(limits, len = 2L, null.ok = TRUE)
      checkmate::assert_logical(interpolate, len=1L, null.ok = TRUE)

      # uses active bindings to validate inputs
      self$name <- name
      self$color_scale <- color_scale
      self$show_legend <- show_legend
      self$definition <- definition

      # allow for empty layers with data added later
      self$data <- data

      if (!is.null(interpolate)) {
        private$pvt_interpolate <- interpolate
      }

      if (!is.null(limits)) {
        self$set_limits(limits)
      }

      if (!is.null(breaks)) {
        self$set_breaks(breaks)
      }
    },

    #' @description set the limits for this layer's scale
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's scale
    set_limits = function(limits) {
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_color_scale$limits <- limits
      return(self)
    },

    #' @description set the breaks element of this layer's scale
    #' @param breaks a function used to label the breaks
    set_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_color_scale$breaks <- breaks
    },

    #' @description plot this layer alone (mostly for debugging)
    plot = function() {
      if (self$is_empty()) stop("Cannot generate plot because this layer has no data! Use $data to add.")

      g <- ggplot(data = private$pvt_data, aes(x=dim1, y=dim2, fill=value)) +
        geom_raster(show.legend = private$pvt_show_legend, interpolate = private$pvt_interpolate) +
        private$pvt_color_scale
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
      new_val <- paste0("value", n_layers + 1L) # new_scale_fill depends on the fill aesthetic mapping differing by layer
      df <- private$pvt_data %>%
        dplyr::rename(!!new_val := value)

      if (n_layers == 0L) {
        base_gg +
          geom_raster(data = df, mapping = aes_string(x="dim1", y="dim2", fill=new_val), show.legend = private$pvt_show_legend) +
          private$pvt_color_scale
      } else {
        base_gg +
          ggnewscale::new_scale_fill() +
          geom_raster(data = df, mapping = aes_string(x="dim1", y="dim2", fill=new_val), show.legend = private$pvt_show_legend) +
          private$pvt_color_scale
      }
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
