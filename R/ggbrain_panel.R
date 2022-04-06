#' R6 class for a single panel of a ggbrain image
#' @importFrom checkmate assert_class
#' @importFrom dplyr bind_rows
#' @export
ggbrain_panel <- R6::R6Class(
  classname = "ggbrain_panel",
  private = list(
    ggobj = NULL,
    pvt_layer_objs = NULL, # list of layers
    pvt_title = NULL,
    generate_ggplot = function() {
      blank_gg <- ggplot(mapping = aes(x=dim1, y=dim2))
      empty <- sapply(private$pvt_layer_objs, function(x) x$is_empty())
      to_plot <- private$pvt_layer_objs[!empty]
      
      # use reduce to add layers from left to right in the list
      gg <- Reduce("+", to_plot, init=blank_gg)
      
      if (!is.null(private$pvt_title)) {
        gg <- gg + ggtitle(private$pvt_title)
      }
      
      private$ggobj <- gg
    }
  ),
  public = list(
    #' @description create a new ggbrain_panel object
    #' @param layers a list of ggbrain_layer objects to form the panel
    #' @param title a title for the panel added to the ggplot object using ggtitle()
    initialize = function(layers = NULL, title = NULL) {
      # convert singleton layer object into a list
      if (checkmate::test_class(layers, "ggbrain_layer")) {
        layers <- list(layers)
      }
      
      # enforce that layers is a named list of ggbrain_layer objects
      checkmate::assert_list(layers)
      sapply(layers, function(x) checkmate::assert_class(x, "ggbrain_layer"))
      layer_names <- sapply(layers, function(x) x$get_name())
      if (any(duplicated(layer_names))) {
        stop("Layer names must be unique but are: ", paste(layer_names, collapse=", "))
      } else {
        names(layers) <- layer_names # for easy referencing
      }
      
      private$pvt_layer_objs <- layers
      
      if (!is.null(title)) {
        checkmate::assert_string(title)
        private$pvt_title <- title
      }
      private$generate_ggplot()
    },
    reset_limits = function(layer_names) {
      
    },
    
    plot = function(use_global_limits = TRUE) {
      # add enforcement of limits
      plot(private$ggobj)
    },
    
    #' @description returns the ggplot object for this panel
    get_gg = function() {
      private$ggobj
    },
    
    #' @description adds a ggplot_layer object to the panel
    #' @param layer_obj a ggbrain_layer object to add to the panel
    add_layer = function(layer_obj) {
      checkmate::assert_class(layer_obj, "ggbrain_layer")
      lname <- layer_obj$get_name()
      if (lname %in% names(private$pvt_layer_objs)) {
        stop("A layer of this name (", lname, ") already exists in the panel.")
      } else {
        private$pvt_layer_objs[[lname]] <- layer_obj
      }
      private$generate_ggplot()
    },
    
    #' @description removes one or more layers by name
    #' @param layer_names a character string of the layers to remove from the panel
    remove_layers = function(layer_names) {
      checkmate::assert_character(layer_names)
      nonexistent_layers <- setdiff(layer_names, names(private$pvt_layer_objs))
      if (length(nonexistent_layers) > 0L) {
        stop("The following layers were requested for removal, but do not exist in the object: ", paste(nonexistent_layers, collapse=", "))
      }
      to_keep <- setdiff(names(private$pvt_layer_objs), layer_names)
      private$pvt_layer_objs <- private$pvt_layer_objs[to_keep]
      private$generate_ggplot()
    },
    
    #' @description returns the data for all layers in the object
    get_data = function() {
      dplyr::bind_rows(
        lapply(private$pvt_layer_objs, function(x) x$get_data(add_layer_name = TRUE))
      )
    },
    
    set_gg = function(ggobj) {
      checkmate::assert_class(ggobj, "gg")
      private$ggobj <- ggobj
    },
    
    #' @description returns the names of the layers in this panel, ordered from bottom to top
    get_layer_names = function() {
      names(private$pvt_layer_objs)
    },
    
    #' @description sets the order of layers from bottom to top based on the layer names provided
    #' @param ordered_names the names of the layers in the desired order from bottom to top. All layer names
    #'   must be provided, not just a subset
    set_layer_order = function(ordered_names=NULL) {
      checkmate::assert_character(ordered_names)
      unused_names <- setdiff(names(private$pvt_layer_objs), ordered_names)
      if (length(unused_names) > 0L) {
        stop("The following layers were not provided in the order: ", paste(unused_names, collapse=", "))
      }
      
      # reorder according to user specification
      private$pvt_layer_objs <- private$pvt_layer_objs[ordered_names]
      private$generate_ggplot() # regenerate object
    }
    
  )
)

# allow for gg + theme() type stuff
`+.ggbrain_panel` <-  function(panel, args) {
  panel_new <- panel$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
  new_plot <- gg_new$get_gg() + args # add args to panel
  panel_new$set_gg(new_plot)
  browser()
  return(panel_new)
}
