#' R6 class for a single layer of a ggbrain panel using fill geom
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes
#' @export
ggbrain_layer_brain <- R6::R6Class(
  classname = "ggbrain_layer_brain",
  inherit = ggbrain_layer,
  private = list(
    # for a brain layer, we just get the underlying data
    get_plot_data = function() {
      data_list <- super$get_plot_data() # general data
      return(data_list)
    }
  ),
  active = list(
    #' @field fill controls color of the filled in pixels for non-NA (valid) voxels. Note that this
    #'   *sets* the fill color, while the mapping=aes(fill=<value>) would *map* the fill to a column
    #'  in the data, consistent with ggplot2 logic.
    fill = function(value) {
      if (missing(value)) {
        private$pvt_fill
      } else {
        checkmate::assert_string(value)
        private$pvt_fill <- value
        private$pvt_has_fill <- TRUE
      }
    },

    #' @field mapping the ggplot2 aesthetic mapping between the data columns and the display
    #' @details To set mapping, you must provide a ggplot2 aes() object. A geom_brain() layer requires
    #'   a `fill` aesthetic mapping, which controls the fill color of regions.
    mapping = function(value) {
      if (missing(value)) {
        private$pvt_mapping
      } else {
        checkmate::assert_class(value, "uneval")
        private$pvt_mapping <- value
        if (is.null(value$fill)) {
          private$pvt_fill_column <- NULL
          private$pvt_has_fill <- FALSE
        } else {
          private$pvt_fill_column <- rlang::as_name(value$fill) # pull out the fill column from aes
          private$pvt_has_fill <- TRUE
        }
        if (!is.null(value$outline)) {
          stop("geom_brain() objects only support a fill aesthetic mapping: aes(fill=<value>)")
        }
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
    #'   `fill` (color of filled pixels). Default is `aes(fill=value)`, which maps the numeric value of the layer data
    #'   to the fill color of the squares at each spatial position. For labeled data, you might use aes(fill=<label_col_name>).
    #' @param fill A character string indicating the color used to fill all non-NA pixels in this layer. This is used in
    #'   distinction to mapping=aes(fill=<variable>).
    #' @param fill_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to smooth it (to make the visual less jagged)
    initialize = function(name = NULL, definition = NULL, data = NULL, 
      limits = NULL, breaks = NULL, show_legend = TRUE, interpolate = NULL, unify_scales=TRUE, alpha = NULL,
      mapping = ggplot2::aes(fill=value), fill = NULL, fill_scale = NULL, blur_edge = NULL) {

      # common initialization steps
      super$initialize(name, definition, data, limits, breaks, show_legend, interpolate, unify_scales, alpha, blur_edge)

      # fill-specific initialization steps
      if (!is.null(fill)) self$fill <- fill # fixed fill
      self$fill_scale <- fill_scale

      if (!is.null(mapping)) self$mapping <- mapping # aesthetic mapping
    }
  )
)

