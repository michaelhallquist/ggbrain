ggbrain_panel <- R6::R6Class(
  classname = "ggbrain_panel",
  private = list(
    pos_limits = c(),
    neg_limits = c(),
    img_df = NULL,
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
    set = function(ggobj) {
      checkmate::assert_class(ggobj, "gg")
      private$ggobj <- ggobj
    }
  )
)
