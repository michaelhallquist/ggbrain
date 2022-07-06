#' R6 class for adding labels to a ggbrain_panel
#' @importFrom checkmate assert_class
#' @importFrom dplyr bind_rows
#' @export
ggbrain_label <- R6::R6Class(
  classname = "ggbrain_label",
  private = list(
    pvt_label_df = NULL,
    pvt_geom = NULL,
    pvt_addl_args = NULL
  ),
  public = list(
    #' @description create a new ggbrain_label object
    #' @param label_df a data.frame containing labels to be printed on the panel. Must contain dim1, dim2, and label as columns.
    #'   The dim1 and dim2 columns control where the labels will appear on the panel
    #' @param geom The geom type to be plotted. Must be "text" or "label", corresponding to geom_text and geom_label, respectively.
    #' @param ... All other arguments that will be passed directly to geom_text or geom_label such as hjust, size, and color
    initialize = function(label_df = NULL, geom="text", ...) {
      private$pvt_addl_args <- list(...)
      checkmate::assert_data_frame(label_df)
      if (!all(c("dim1", "dim2", "label") %in% names(label_df))) {
        stop("label_df must contain dim1, dim2, and label")
      }
      checkmate::assert_string(geom)
      checkmate::assert_subset(geom, c("text", "label"))
      private$pvt_label_df <- label_df
      private$pvt_geom <- geom
    },
    
    #' @description add this annotation to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)
      
      # return the modified ggplot object with the labels added
      base_gg +
        do.call(paste0("geom_", private$pvt_geom), 
                args = c(list(data=private$pvt_label_df, mapping=aes_string(x="dim1", y="dim2", label="label")), private$pvt_addl_args)
        )
    }
  )
)

#' S3 method to support adding ggbrain_label objects to an existing ggplot object
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_label <- function(object, plot, object_name) {
  object$add_to_gg(plot) # adds the layer to the extant plot
}
