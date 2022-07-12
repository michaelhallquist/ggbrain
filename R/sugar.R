# Sugar functions for making the ggbrain() + layers approach work

#' create ggb container object for a given plot
#' @param images a character vector or existing ggbrain_images object defining which
#'   images should be included in this plot
#' @param labels a list of data.frames for labels?
#' @param slices a set of slices to be added to the plot
#' @export
ggbrain <- function(images = NULL, labels = NULL, slices = NULL) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
  } else {
    img_obj <- ggbrain_images$new(images)
  }

  if (!is.null(labels)) {

  }

  if (!is.null(slices)) {
    img_obj$reset_slices(slices)
    img_obj$add_slices(slices)
  }

  ggb_obj <- ggb$new(images = img_obj)

  # get_slices = function(slices, img_names = NULL, contrasts = NULL, make_square = TRUE, remove_null_space = TRUE) {
  return(ggb_obj)
}

#' Generic R6 base class that is used to support + semantics
#' @details this object becomes a simple storage class that contains all relevant objects
#'   (e.g., ggbrain_images) required to generate a brain plot
#' @importFrom checkmate assert_class
#' @keywords internal
ggb <- R6::R6Class(
  "ggb",
  private=list(),
  public=list(
    #' @field ggb_images ggbrain_images object for this plot
    ggb_images = NULL,

    #' @field ggb_slices character string of slices to extract
    ggb_slices = NULL,

    #' @field ggb_layers a list of ggbrain_layer objects containing the bottom-to-top layers to be plotted
    ggb_layers = NULL,

    #' @field ggb_plot a ggbrain_plot object containing the specification of the plot
    ggb_plot = NULL,

    #' @field action what should this ggb object contribute to another when added with it?
    action = NULL,

    #' @description create a new ggb object. Note that inputs are always cloned to avoid
    #'   unintended modify-in-place behaviors of R6 classes.
    #' @param images a ggbrain_images object containing relevant images
    #' @param slices a character vector of slices to extract
    #' @param layers a list of ggbrain_layer objects
    #' @param action the action to be taken when adding this object to an existing ggb
    initialize=function(images=NULL, slices=NULL, layers = NULL, action=NULL) {
      if (!is.null(images)) {
        checkmate::assert_class(images, "ggbrain_images")
        self$ggb_images <- images$clone(deep=TRUE)
      }

      if (!is.null(slices)) {
        checkmate::assert_character(slices)
        self$ggb_slices <- slices
      }

      if (!is.null(layers)) {
        if (checkmate::test_class(layers, "ggbrain_layer")) {
          layers <- list(layers) # form one-element list
        }

        # should be a list of layer objects
        sapply(layers, function(x) checkmate::assert_class(x, "ggbrain_layer"))
        self$ggb_layers <- layers
      }

      # for tracking addition actions
      if (!is.null(action)) {
        self$action <- action
      }
    },

    #' @description add layers from another ggb object to this one
    #' @param ilist a list of ggbrain_layer objects. If a ggb object is passed, we
    #'   will get this list from obj$ggb_layers
    add_layer = function(ilist) {
      if (checkmate::test_class(ilist, "ggb")) {
        ilist <- ilist$ggb_layers
      }
      checkmate::assert_list(ilist)

      self$ggb_layers <- c(self$ggb_layers, ilist)
      return(self)
    },

    #' @description add slices to the existing vector of slices
    #' @param slices a character vector of slices to be appended to the existing slices
    add_slices = function(slices=NULL) {
      checkmate::assert_character(slices)
      self$ggb_slices <- c(self$ggb_slices, slices)
      return(self)
    },

    render = function() {
      img <- self$ggb_images$clone(deep = TRUE)
      img$add_slices(self$ggb_slices)
      img$add_contrasts(self$ggb_contrasts)
      slc <- img$get_slices()
      plot_obj <- ggbrain_plot$new(slc)
      plot_obj$layers <- self$ggb_layers
      plot_obj$generate_plot()
      plot_obj$plot()

    },

    #' @description plot this ggb object
    #' @details requires that required elements are in place already.
    plot = function() {

      # integrate images

      # get slices

      # get contrasts

      # setup ggbrain_plot object with layers

    }
  )
)

#' addition operator for ggb object to support ggplot-like syntax
#' @param o1 the first object inheriting the ggb class
#' @param o2 the second object inheriting the ggb class
#' @return a modified version of the o1 object with o2 added to it
#' @details Note that the addition operator always clones the underlying o1 object
#'   rather than modifying it in place
#' @export
`+.ggb` <- function(o1, o2) {
  if (is.null(o2$action)) {
    # nothing to do
    return(o1)
  } else {
    oc <- o1$clone(deep=TRUE)
    oc$action <- NULL # always make sure no action is needed in combined object
    if (o2$action == "add_slices") {
      oc$add_slices(o2$ggb_slices)
    } else if (o2$action == "add_images") {
      # use direct addition approach (by reference) -- yields single ggbrain_images object
      oc$ggb_images$add(o2$ggb_images)
    } else if (o2$action == "add_layer") {
      oc$add_layer(o2$ggb_layers)
    } else if (o2$action == "render") {
      oc$render()
    }
  }

  return(oc)
}

### Lower-level sugar functions for adding plot elements
### Each returns a ggb object with the relevant action for adding it to the overall ggb object

#' Add slices to a ggb object
#' @param slices a character vector of slices to be added to the ggb plot object
#' @return a ggb object with the relevant slices and an action of 'add_slices'
#' @export
add_slices <- function(slices = NULL) {
  if (is.null(slices)) {
    ret <- ggb$new(action = NULL)
  } else {
    checkmate::assert_character(slices)
    ret <- ggb$new(slices = slices, action = "add_slices")
  }
  return(ret)
}

#' Add images to a ggb object
#' @param images a character vector or ggbrain_images object containing NIfTI images to add to this plot
#' @return a ggb object with the relevant images and an action of 'add_images'
#' @export
add_images <- function(images = NULL) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
  } else {
    img_obj <- ggbrain_images$new(images)
  }

  ret <- ggb$new(images = img_obj, action = "add_images")

  return(ret)
}

#' Helper function to add a contrast layer to the plot. This is a thin wrapper around ggbrain_layer
#' @param ... all parameters are passed through to ggbrain_layer$new() to create a new layer object
#' @details Note that the color_scale and limits must be specified at the time of the geom_contrast creation
#'   in order for them to be mapped properly within ggplot. Because we overlay many raster layers in a ggplot
#'   object that all use the fill aesthetic mapping, it becomes hard to map the color scales after the layer is
#'   created using the typical + scale_fill_X syntax, and similarly for scale limits.
#' @return a ggb object populated with the ggb_layer and the action of 'add_layer'
#' @export
geom_brain <- function(...) {
  inp_args <- list(...)
  l_obj <- do.call(ggbrain_layer$new, inp_args)

  ret <- ggb$new(layers = l_obj, action="add_layer")
}
