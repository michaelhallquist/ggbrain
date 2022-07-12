#' R6 class for a single panel of a ggbrain image
#' @importFrom checkmate assert_class
#' @importFrom dplyr bind_rows
#' @export
ggbrain_panel <- R6::R6Class(
  #classname = c("ggbrain_panel", "gg", "ggplot"),
  classname = "ggbrain_panel",
  private = list(
    ggobj = NULL,
    pvt_annotation_objs = NULL, # list of annotation objects to add
    pvt_layer_objs = NULL, # list of layers
    pvt_title = NULL,
    pvt_addl = list(), # any custom additional objects to add, as a list
    pvt_base_size = 14, # font size
    pvt_bg_color = "gray10", # nearly black
    pvt_text_color = "white",
    pvt_border_color = "gray60", # default panel border color, if requested
    pvt_border_size = 1.1,
    pvt_draw_border = FALSE,
    pvt_xlab = NULL,
    pvt_ylab = NULL,
    pvt_theme_custom = NULL,
    pvt_default_theme = NULL,
    
    # helper function to initialize default theme. needs to be executed after
    # private list is initialized to avoid failed cross-referencing
    set_default_theme = function() {
      private$pvt_default_theme <- list(
        theme_void(base_size = private$pvt_base_size),
        coord_fixed(),
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          plot.background = element_rect(
            fill = private$pvt_bg_color,
            color = ifelse(isTRUE(private$pvt_panel_border), private$pvt_border_color, NA),
            size = private$pvt_border_size
          ),
          panel.background = element_rect(fill=private$pvt_bg_color, color=NA),
          text = element_text(color = private$pvt_text_color),
          legend.spacing.y = unit(0.1, "lines"),
          legend.position = "right",
          plot.margin = unit(c(0.0, 0.5, 0.0, 0.5), "lines") # space on L and R, but not T and B
        )
      )
    },
    generate_ggplot = function() {
      blank_gg <- ggplot(mapping = aes(x=dim1, y=dim2))
      empty <- sapply(private$pvt_layer_objs, function(x) x$is_empty())
      to_plot <- private$pvt_layer_objs[!empty]

      # use reduce to add layers from left to right in the list
      gg <- Reduce("+", to_plot, init=blank_gg)

      # always start with default panel theme
      gg <- gg + private$pvt_default_theme + private$pvt_theme_custom

      if (!is.null(private$pvt_title)) {
        gg <- gg + ggtitle(private$pvt_title)
      }

      if (!is.null(private$pvt_xlab)) {
        gg <- gg + theme(axis.title.x = element_text()) + xlab(private$pvt_xlab)
      }

      if (!is.null(private$pvt_ylab)) {
        gg <- gg + theme(axis.title.y = element_text()) + ylab(private$pvt_ylab)
      }

      gg <- gg + private$pvt_addl

      self$gg <- gg
    }
  ),
  public = list(
    #' @field gg The ggplot object that contains the panel
    gg = NULL, # ggplot object
    
    #' @description create a new ggbrain_panel object
    #' @param layers a list of ggbrain_layer objects to form the panel
    #' @param title a title for the panel added to the ggplot object using ggtitle()
    #' @param bg_color the color used for the background of the plot. Default: 'gray10' (nearly black)
    #' @param text_color the color used for text displayed on the plot. Default: 'white'.
    #' @param border_color the color used for drawing a border around on the plot. Default: 'gray50' 
    #'   (though borders are not drawn by default).
    #' @param border_size the size of the border line drawn around the panel. Default: 1.1.
    #' @param draw_border if TRUE, a panel border of color \code{border_color} and \code{border_size} is
    #'   drawn around the plot. Default: FALSE
    #' @param xlab The label to place on x axis. Default is NULL.
    #' @param ylab The label to place on y axis. Default is NULL.
    #' @param theme_custom Any custom theme() settings to be added to the plot
    initialize = function(
      layers = NULL, title = NULL, bg_color = NULL, text_color = NULL, border_color = NULL, border_size = NULL,
      draw_border = NULL, xlab = NULL, ylab = NULL, theme_custom = NULL
    ) {
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
      
      if (!is.null(bg_color)) {
        checkmate::assert_string(bg_color)
        private$pvt_bg_color <- bg_color
      }
      
      if (!is.null(text_color)) {
        checkmate::assert_string(text_color)
        private$pvt_text_color <- text_color
      }
      
      if (!is.null(border_color)) {
        checkmate::assert_string(border_color)
        private$pvt_border_color <- border_color
      }
      
      if (!is.null(border_size)) {
        checkmate::assert_number(border_size, lower = 0.001)
        private$pvt_border_size <- border_size
      }
      
      if (!is.null(draw_border)) {
        checkmate::assert_logical(draw_border, len=1L)
        private$pvt_draw_border <- draw_border
      }
      
      if (!is.null(xlab)) {
        checkmate::assert_string(xlab)
        private$pvt_xlab <- xlab
      }
      
      if (!is.null(ylab)) {
        checkmate::assert_string(ylab)
        private$pvt_ylab <- ylab
      }
      
      if (!is.null(theme_custom)) {
        checkmate::assert_class(theme_custom, "theme")
        private$pvt_theme_custom <- theme_custom
      }
      
      # populate default theme object (propagates background and text color)
      private$set_default_theme()
      
      private$generate_ggplot()
    },
    
    #' @description Reset the scale limits for the specified layers
    #' @param layer_names not implemented yet
    reset_limits = function(layer_names) {
      
    },
    
    #' @description plot the panel
    #' @param use_global_limits Not implemented at present
    plot = function(use_global_limits = TRUE) {
      # add enforcement of limits
      plot(self$gg)
    },
    
    #' @description add one or more custom ggplot settings to the panel
    #' @param list_args A list containing elements to add to the ggplot object
    #' @details Note that passing in an expression such as theme_bw() + ggtitle("hello")
    #'   will not work because it creates an object that cannot be added sequentially to
    #'   the ggplot. As noted in ggplot2's documentation (https://ggplot2.tidyverse.org/reference/gg-add.html),
    #'   to programmatically add elements to a ggplot, pass in a list where each element is added sequentially
    add_to_gg = function(list_args) {
      checkmate::assert_list(list_args)
      
      # always append custom arguments
      private$pvt_addl <- c(private$pvt_addl, list_args)
      private$generate_ggplot() # regenerate the plot
      return(self)
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

    #' @description returns the names of the layers in this panel, ordered from bottom to top
    get_layer_names = function() {
      names(private$pvt_layer_objs)
    },
    
    #' @description returns a list of ggbrain_layer objects that comprise this panel
    get_layers = function() {
      private$pvt_layer_objs
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

# allow for panel + ggplot() stuff -- this doesn't work as expected due to S3 precedence problems
# @export
# `+.ggbrain_panel` <-  function(panel, args) {
#   panel_new <- panel$clone(deep=TRUE) # need a new object to modify the panels in memory (not by reference)
#   new_plot <- panel_new$get_gg() + args # add args to panel
#   panel_new$set_gg(new_plot)
#   browser()
#   return(panel_new)
# }


# this supports adding a ggbrain_panel object to an existing ggplot object, but that's not really
# the most useful. And having this here means that the S3 dispatch gets tripped up when we
# try to add 

# tests
# ggplot() + panel_obj
# panel_obj + theme_void()
# panel_obj + ggtitle("hello")
# 
# sloop::s3_dispatch(panel_obj + theme_void())
# sloop::s3_dispatch(panel_obj + ggtitle("hello"))
# sloop::s3_dispatch(ggplot() + panel_obj$get_layers()[[1]])
# sloop::s3_dispatch(ggplot() + panel_obj)

#' S3 method to support adding ggbrain_layer objects to an existing ggplot object
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggbrain_panel <- function(object, plot, object_name) {
  plot + object$gg
}


#' S3 method to allow for plot() syntax with ggbrain_panel objects
#' @export
plot.ggbrain_panel <- function(object) {
  object$plot()
}
