#' R6 class for a single panel of a ggbrain image
#' @importFrom checkmate assert_class
#' @export
ggbrain_panel <- R6::R6Class(
  classname = "ggbrain_panel",
  private = list(
    pos_limits = c(),
    neg_limits = c(),
    pvt_img_df = NULL, # data.frame of data for panel, layer indicates bottom-top order
    pvt_layer_scales = list(),
    pvt_layer_limits = list(),
    ggobj = NULL
  ),
  public = list(
    initialize = function(ggobj = NULL, neg_limits = NULL, pos_limits = NULL) {
      checkmate::assert_class(ggobj, "gg")
      private$ggobj <- ggobj
    },
    plot = function(use_global_limits = TRUE) {
      # add enforcement of limits
      plot(private$ggobj)
    },
    get = function() {
      private$ggobj
    },
    get_data = function() {
      private$pvt_img_df
    },
    set = function(ggobj) {
      checkmate::assert_class(ggobj, "gg")
      private$ggobj <- ggobj
    }
  )
)
