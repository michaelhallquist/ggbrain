#' R6 class for a single layer of a ggbrain panel
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_layer_df = NULL,
    pvt_layer_scale = NULL,
    pvt_show_scale = NULL,
    pvt_interpolate = FALSE
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param layer_df the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    #' @param layer_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param layer_limits if provided, sets the upper and lower bounds on the scale
    #' @param show_scale if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    initialize = function(layer_df = NULL, layer_scale = NULL, layer_limits = NULL, show_scale = TRUE, interpolate = NULL) {
      checkmate::assert_data_frame(layer_df)
      checkmate::assert_class(layer_scale, "Scale")
      checkmate::assert_numeric(layer_limits, len = 2L, null.ok = TRUE)
      checkmate::assert_logical(show_scale, len=1L)
      checkmate::assert_logical(interpolate, len=1L, null.ok = TRUE)
      if (!all(c("dim1", "dim2", "value") %in% names(layer_df))) {
        stop("layer_df must contain dim1, dim2, and value")
      }
      
      private$pvt_layer_df <- layer_df
      private$pvt_layer_scale <- layer_scale
      private$pvt_show_scale <- show_scale
      if (!is.null(interpolate)) {
        private$pvt_interpolate <- interpolate
      }
      
      if (!is.null(layer_limits)) {
        self$set_limits(layer_limits)
      }
    },
    #' @description set the limits for this layer's scale
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's scale
    set_limits = function(limits) {
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_layer_scale$limits <- limits
      return(self)
    },
    #' @description plot this layer alone (mostly for debugging)
    plot = function() {
      g <- ggplot(data = private$pvt_layer_df, aes(x=dim1, y=dim2, fill=value)) +
        geom_raster(show.legend = private$pvt_show_scale, interpolate = private$pvt_interpolate) +
        private$pvt_layer_scale
      return(g)
    },
    #' @description method to add this layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_layer = function(base_gg) {
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      new_val <- paste0("value", n_layers + 1L)
      df <- private$pvt_layer_df %>%
        dplyr::rename(!!new_val := value)
      
      if (n_layers == 0L) {
        base_gg + 
          geom_raster(data = df, mapping = aes_string(x="dim1", y="dim2", fill=new_val), show.legend = private$pvt_show_scale) +
          private$pvt_layer_scale  
      } else {
        base_gg + 
          new_scale_fill() +
          geom_raster(data = df, mapping = aes_string(x="dim1", y="dim2", fill=new_val), show.legend = private$pvt_show_scale) +
          private$pvt_layer_scale  
      }
    }
  )
)

#' S3 method to support adding ggbrain_layer objects to an existing ggplot object
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_layer <- function(object, plot, object_name) {
  object$add_layer(plot) # adds the layer to the extant plot
}
