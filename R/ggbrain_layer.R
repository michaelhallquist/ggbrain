#' R6 class for a single layer of a ggbrain panel
#' @importFrom checkmate assert_data_frame assert_class assert_numeric assert_logical
#' @export
ggbrain_layer <- R6::R6Class(
  classname = "ggbrain_layer",
  private = list(
    pvt_layer_name = NULL,
    pvt_data = NULL,
    pvt_layer_scale = NULL,
    pvt_show_scale = NULL,
    pvt_interpolate = FALSE,
    pvt_is_empty = NULL
  ),
  public = list(
    #' @description create a new ggbrain_layer object
    #' @param name the name of this layer, used for referencing in layer and panel modifications
    #' @param data the data.frame containing image data for this layer. Must contain "dim1", "dim2",
    #'   and "value" as columns
    #' @param layer_scale a ggplot scale object used for mapping the value column as the fill color for the
    #'   layer.
    #' @param limits if provided, sets the upper and lower bounds on the scale
    #' @param breaks if provided, a function to draw the breaks on the color scale
    #' @param show_scale if TRUE, show the scale on the plot legend
    #' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
    initialize = function(name = NULL, data = NULL, layer_scale = NULL, limits = NULL, 
                          breaks = NULL, show_scale = TRUE, interpolate = NULL) {
      if (is.null(name)) name <- "layer"
      checkmate::assert_string(name)
      checkmate::assert_data_frame(data)
      checkmate::assert_class(layer_scale, "Scale")
      checkmate::assert_numeric(limits, len = 2L, null.ok = TRUE)
      checkmate::assert_logical(show_scale, len=1L)
      checkmate::assert_logical(interpolate, len=1L, null.ok = TRUE)
      if (!all(c("dim1", "dim2", "value") %in% names(data))) {
        stop("data must contain dim1, dim2, and value")
      }
      
      private$pvt_layer_name <- name
      private$pvt_data <- data
      private$pvt_layer_scale <- layer_scale
      private$pvt_show_scale <- show_scale
      if (!is.null(interpolate)) {
        private$pvt_interpolate <- interpolate
      }
      
      if (!is.null(limits)) {
        self$set_limits(limits)
      }
      
      if (!is.null(breaks)) {
        self$set_breaks(breaks)
      }
      
      # encode whether this layer is empty so that it doesn't get added to the panel
      if (nrow(data) == 0L || all(is.na(data$value))) {
        private$pvt_is_empty <- TRUE
      } else {
        private$pvt_is_empty <- FALSE
      }
    },
    
    #' @description set the limits for this layer's scale
    #' @param limits a 2-element numeric vector setting the lower and upper limits on the layer's scale
    set_limits = function(limits) {
      checkmate::assert_numeric(limits, len=2L)
      private$pvt_layer_scale$limits <- limits
      return(self)
    },
    
    #' @description set the breaks element of this layer's scale
    #' @param breaks a function used to label the breaks
    set_breaks = function(breaks) {
      checkmate::assert_class(breaks, "function")
      private$pvt_layer_scale$breaks <- breaks
    },
    
    #' @description set the layer name
    #' @param name a character string defining the layer's name
    set_name = function(name) {
      checkmate::assert_string(name)
      private$pvt_layer_name <- name
    },
    
    #' @description plot this layer alone (mostly for debugging)
    plot = function() {
      g <- ggplot(data = private$pvt_data, aes(x=dim1, y=dim2, fill=value)) +
        geom_raster(show.legend = private$pvt_show_scale, interpolate = private$pvt_interpolate) +
        private$pvt_layer_scale
      return(g)
    },
    
    #' @description method to add this layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      new_val <- paste0("value", n_layers + 1L) # new_scale_fill depends on the fill aesthetic mapping differing by layer
      df <- private$pvt_data %>%
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
    },
    
    #' @description return the data.frame associated with this layer
    #' @param add_layer_name if TRUE, adds a \code{layer_name} column to the data.frame for record-keeping.
    #'   Default: FALSE.
    get_data = function(add_layer_name = FALSE) {
      if (isTRUE(add_layer_name)) {
        df <- private$pvt_data
        df$layer_name <- private$pvt_layer_name
        return(df)
      } else {
        return(private$pvt_data)
      }
    },
    
    #' @description return the layer name
    get_name = function() {
      private$pvt_layer_name
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
