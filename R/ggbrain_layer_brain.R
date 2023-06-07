#' @title R6 class for a single layer of a ggbrain panel using fill geom
#' @details 
#'   Note that this class is exported only for power users and rarely needs to be called directly
#'     in typical use of the package. Instead, look at `geom_brain()`.
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @importFrom ggplot2 scale_fill_gradient scale_fill_distiller .pt aes
#' @importFrom rlang as_label
#' @return a `ggbrain_layer_brain` R6 class with fields related to a brain visual layer (relates to `geom_brain`)
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
    #'   \strong{sets} the fill color, while the \code{mapping=aes(fill=<value>)} would \strong{map} the fill to a column
    #'  in the data, consistent with ggplot2 logic.
    fill = function(value) {
      if (missing(value)) {
        private$pvt_fill
      } else {
        checkmate::assert_string(value)
        private$pvt_fill <- value
        private$pvt_has_fill <- TRUE
        private$pvt_fill_column <- NULL
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
          private$pvt_fill_column <- all.vars(value$fill) # pull out the fill column from aes
          if (length(private$pvt_fill_column) > 1L) stop("Cannot pass multiple columns as a fill mapping")

          # if the user modifies the fill column such as as.factor(), we need to carry through the expression to geom_raster
          # inside the add_raster method of ggbrain_layer, we use 'new_val' as the column name to plot. Create a glue expression here
          # that will be evaluated at the time of the add_raster.
          fill_expr <- rlang::as_label(value$fill)
          if (fill_expr != private$pvt_fill_column) {
            private$pvt_fill_glue <- sub(private$pvt_fill_column, "{new_val}", fill_expr, fixed=TRUE)
          }
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
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_legend if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    #' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
    #' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
    #' @param mapping the aesthetic mapping of the layer data to the display. Should be an aes() object and supports
    #'   `fill` (color of filled pixels). Default is `aes(fill=value)`, which maps the numeric value of the layer data
    #'   to the fill color of the squares at each spatial position. For labeled data, you might use \code{aes(fill=<label_col_name>)}.
    #' @param fill A character string indicating the color used to fill all non-NA pixels in this layer. This is used in
    #'   distinction to \code{mapping=aes(fill=<variable>)}.
    #' @param fill_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
    #'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
    #' @param fill_holes the size of holes (in pixels) inside clusters to be filled by nearest neighbor imputation prior to display
    #' @param remove_specks the size of specks (in pixels) to be removed from each slice prior to display
    #' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    initialize = function(name = NULL, definition = NULL, 
      limits = NULL, breaks = integer_breaks(), show_legend = TRUE, interpolate = NULL, unify_scales=TRUE, alpha = NULL,
      mapping = ggplot2::aes(fill=value), fill = NULL, fill_scale = NULL, blur_edge = NULL,
      fill_holes = NULL, remove_specks = NULL, trim_threads = NULL, data = NULL) {

      # common initialization steps
      super$initialize(
        name, definition, limits, breaks, show_legend, interpolate, unify_scales,
        alpha, blur_edge, fill_holes, remove_specks, trim_threads, data
      )

      # fill-specific initialization steps
      if (!is.null(fill)) self$fill <- fill # fixed fill
      self$fill_scale <- fill_scale

      if (!is.null(mapping)) self$mapping <- mapping # aesthetic mapping
    }
  )
)
