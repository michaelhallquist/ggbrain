#' R6 class for adding labels to a ggbrain_panel
#' @importFrom checkmate assert_class
#' @importFrom dplyr bind_rows
#' @export
ggbrain_label <- R6::R6Class(
  classname = "ggbrain_label",
  private = list(
    pvt_data = NULL, # data.frame containing label data with coordinates of label positions
    pvt_image = NULL, # placeholder for image that contains relevant slice data
    pvt_geom = NULL,
    pvt_addl_args = NULL,
    pvt_label_column = "label"
  ),
  active = list(
    data = function(value) {
      if (missing(value)) {
        return(private$pvt_data)
      } else if (is.null(value)) {
        private$pvt_data <- NULL
      } else {
        checkmate::assert_data_frame(value)
        if (!all(c("dim1", "dim2") %in% names(value))) {
          stop("data must contain dim1, dim2")
        }
        col_classes <- sum(sapply(value, function(v) inherits(v, c("character", "ordered", "factor"))))
        if (ncol(value) < 1L) {
          stop("At least one character, factor, or ordered label column must be supplied")
        }
        private$pvt_data <- value
      }
    },
    image = function(value) {
      if (missing(value)) {
        return(private$pvt_image)
      } else {
        checkmate::assert_string(value)
        private$pvt_image <- value
      }
    },
    label_column = function(value) {
      if (missing(value)) {
        return(private$pvt_label_column)
      } else {
        checkmate::assert_string(value)
        if (!is.null(private$pvt_data) && !value %in% names(private$pvt_data)) {
          warning(glue::glue("Requested label_column: {value} does not exist in label data.frame"))
        }
        private$pvt_label_column <- value
      }
    }

  ),
  public = list(
    #' @description create a new ggbrain_label object
    #' @param data a data.frame containing labels to be printed on the panel. Must contain dim1, dim2, and label as columns.
    #'   The dim1 and dim2 columns control where the labels will appear on the panel
    #' @param geom The geom type to be plotted. Must be "text" or "label", corresponding to geom_text and geom_label, respectively.
    #' @param ... All other arguments that will be passed directly to geom_text or geom_label such as hjust, size, and color
    initialize = function(data = NULL, geom="text", label_column = NULL, ...) {
      private$pvt_addl_args <- list(...)
      self$data <- data
      if (!is.null(label_column)) self$label_column <- label_column
      checkmate::assert_string(geom)
      checkmate::assert_subset(geom, c("text", "label"))
      private$pvt_geom <- geom
    },

    #' @description add this text layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)

      # enforce presence of label column at the stage of adding labels
      if (!private$pvt_label_column %in% private$pvt_data) {
        stop(glue::glue("Requested label_column: {value} does not exist in label data.frame"))
      }

      # return the modified ggplot object with the labels added
      base_gg +
        do.call(paste0("geom_", private$pvt_geom),
                args = c(list(data = private$pvt_data, mapping = aes_string(x = "dim1", y = "dim2", label = private$pvt_label_column)), private$pvt_addl_args)
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
