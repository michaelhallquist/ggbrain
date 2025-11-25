#' R6 class for adding labels to a ggbrain_panel
#' @importFrom checkmate assert_class
#' @importFrom dplyr bind_rows
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom ggplot2 aes
#' @importFrom rlang sym
#' @importFrom R6 R6Class
#' @return a `ggbrain_label` R6 class containing fields related to ggbrain plot labels
#' @export
ggbrain_label <- R6::R6Class(
  classname = "ggbrain_label",
  private = list(
    pvt_data = NULL, # data.frame containing label data with coordinates of label positions
    pvt_image = NULL, # placeholder for image that contains relevant slice data
    pvt_geom = NULL,
    pvt_label_column = "label",
    pvt_min_px = 1L # minimum number of pixels on a slice required to display label
  ),
  active = list(

    #' @field data a data.frame containing labels to be printed on the panel. Must contain dim1, dim2, and label as columns.
    #'   The dim1 and dim2 columns control where the labels will appear on the panel
    data = function(value) {
      if (missing(value)) {
        return(private$pvt_data)
      } else if (is.null(value)) {
        private$pvt_data <- NULL
      } else {
        checkmate::assert_data_frame(value)
        if (!all(c("dim1", "dim2", "n") %in% names(value))) {
          stop("data must contain dim1, dim2, n")
        }
        n_discrete_cols <- sum(sapply(value, function(v) inherits(v, c("character", "ordered", "factor"))))
        if (n_discrete_cols < 1L) {
          stop("At least one character, factor, or ordered label column must be supplied")
        }
        private$pvt_data <- value
      }
    },

    #' @field image A character string specifying the image to which these labels pertain
    image = function(value) {
      if (missing(value)) {
        return(private$pvt_image)
      } else {
        checkmate::assert_string(value)
        private$pvt_image <- value
      }
    },

    #' @field label_column A character string indicating which data.frame column should be used for drawing labels
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
    },
    
    #' @field min_px A positive integer indicating the minimum number of pixels present on slice that will generate a label
    min_px = function(value) {
      if (missing(value)) {
        return(private$pvt_min_px)
      } else {
        checkmate::assert_integerish(value, lower=1L, len=1L)
        private$pvt_min_px <- as.integer(value)
      }
    }
  ),
  public = list(
    #' @field addl_args a named list of additional argument to be passed to geom_text/geom_label at render
    addl_args = NULL,

    #' @description create a new ggbrain_label object
    #' @param data a data.frame containing labels to be printed on the panel. Must contain dim1, dim2, and label as columns.
    #'   The dim1 and dim2 columns control where the labels will appear on the panel
    #' @param geom The geom type to be plotted. Must be "text" or "label", corresponding to geom_text and geom_label, respectively.
    #' @param image A string specifying the image to which these labels pertain
    #' @param label_column the column in \code{data} that should be drawn as labels on the plot
    #' @param min_px the minimum number of pixels 
    #' @param ... All other arguments that will be passed directly to geom_text or geom_label such as hjust, size, and color
    initialize = function(data = NULL, geom="text", image = NULL, label_column = NULL, min_px = NULL, ...) {
      self$addl_args <- list(...)
      self$data <- data
      self$min_px <- min_px

      if (!is.null(label_column)) self$label_column <- label_column
      checkmate::assert_string(geom)
      checkmate::assert_subset(geom, c("text", "label", "text_repel", "label_repel"))
      private$pvt_geom <- geom
      self$image <- image
    },

    #' @description add this text layer to an existing ggplot object
    #' @param base_gg the ggplot object to which we add the layer
    add_to_gg = function(base_gg) {
      # if no data are provided, then there is nothing to label
      if (is.null(private$pvt_data)) return(base_gg)
      checkmate::assert_class(base_gg, "gg")
      n_layers <- length(base_gg$layers)

      # enforce presence of label column at the stage of adding labels
      if (!private$pvt_label_column %in% names(private$pvt_data)) {
        stop(glue::glue("Requested label_column: {private$pvt_label_column} does not exist in label data.frame"))
      }
      
      # subset labels so that only regions with at least min_px pixels on the slice are labeled
      df <- private$pvt_data
      if (self$min_px > 1L) {
        df <- df %>% dplyr::filter(n >= !!self$min_px)
      }
      
      # for now, always na.rm=TRUE to remove warnings about missing labels
      self$addl_args$na.rm <- TRUE
      
      # return the modified ggplot object with the labels added
      base_gg +
        do.call(
          paste0("geom_", private$pvt_geom),
          args = c(
            list(
              data = df,
              mapping = ggplot2::aes(
                x = !!rlang::sym("dim1"),
                y = !!rlang::sym("dim2"),
                label = !!rlang::sym(private$pvt_label_column)
              )
            ),
            self$addl_args
          )
        )
    }
  )
)

#' S3 method to support adding ggbrain_label objects to an existing ggplot object
#' @param object the ggbrain_layer object to be added to an existing ggplot
#' @param plot the ggplot object
#' @param object_name not used, but required by ggplot_add
#' @param ... additional arguments, not currently used
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_label <- function(object, plot, object_name, ...) {
  object$add_to_gg(plot) # adds the layer to the extant plot
}
