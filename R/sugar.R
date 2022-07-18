# Sugar functions for making the ggbrain() + layers approach work

#' create ggb container object for a given plot
#' @param images a character vector or existing ggbrain_images object defining which
#'   images should be included in this plot
#' @param labels a list of data.frames for labels?
#' @param slices a set of slices to be added to the plot
#' @export
ggbrain <- function(images = NULL, labels = NULL, slices = NULL, bg_color="grey8", text_color="grey92") {
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

  ggb_obj <- ggb$new(images = img_obj, bg_color=bg_color, text_color=text_color)

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

    #' @field ggb_labels a named list of data.frames that label corresponding images
    ggb_labels = NULL,

    #' @field ggb_slices character string of slices to extract
    ggb_slices = NULL,

    #' @field ggb_layers a list of ggbrain_layer objects containing the bottom-to-top layers to be plotted
    ggb_layers = NULL,

    #' @field ggb_plot a ggbrain_plot object containing the specification of the plot
    ggb_plot = NULL,

    #' @field ggb_annotation a list of annotation objects
    ggb_annotate = NULL,

    #' @field action what should this ggb object contribute to another when added with it?
    action = NULL,

    #' @description create a new ggb object. Note that inputs are always cloned to avoid
    #'   unintended modify-in-place behaviors of R6 classes.
    #' @param images a ggbrain_images object containing relevant images
    #' @param slices a character vector of slices to extract
    #' @param layers a list of ggbrain_layer objects
    #' @param action the action to be taken when adding this object to an existing ggb
    initialize=function(images=NULL, slices=NULL, layers = NULL, labels = NULL, action=NULL, bg_color="grey50", text_color="black") {
      if (!is.null(images)) {
        checkmate::assert_class(images, "ggbrain_images")
        self$ggb_images <- images$clone(deep=TRUE)
      }

      if (!is.null(slices)) {
        checkmate::assert_character(slices)
        self$ggb_slices <- slices
      }

      if (!is.null(layers)) {
        # form one-element list if a ggbrain_object is passed
        if (checkmate::test_class(layers, "ggbrain_layer")) layers <- list(layers)
        self$add_layers(layers)
      }

      if (!is.null(labels)) {
        checkmate::assert_list(labels, names = "unique")
        do.call(self$add_labels, labels)
      }

      # for tracking addition actions
      if (!is.null(action)) {
        self$action <- action
      }
    },

    #' @description add layers from another ggb object to this one
    #' @param ilist a list of ggbrain_layer objects. If a ggb object is passed, we
    #'   will get this list from obj$ggb_layers
    add_layers = function(ilist) {
      if (checkmate::test_class(ilist, "ggb")) {
        ilist <- ilist$ggb_layers
      }
      checkmate::assert_list(ilist)
      sapply(ilist, function(x) checkmate::assert_class(x, "ggbrain_layer"))

      self$ggb_layers <- c(self$ggb_layers, ilist)

      names(self$ggb_layers) <- sapply(self$ggb_layers, "[[", "name")
      return(self)
    },

    #' @description add slices to the existing vector of slices
    #' @param slices a character vector of slices to be appended to the existing slices
    add_slices = function(slices=NULL) {
      checkmate::assert_character(slices)
      self$ggb_slices <- c(self$ggb_slices, slices)
      return(self)
    },

    #' @description add labels to a given image
    #' @param labels a named list of data.frame objects where the names denote corresponding images
    add_labels = function(...) {
      label_args <- list(...)

      # return unchanged object if no input labels found
      if (is.null(label_args) || length(label_args) == 0L) {
        return(self)
      }
      label_names <- names(label_args)
      if (is.null(label_names) || any(label_names == "")) {
        stop("All arguments must be named, with the name referring to the image to be labeled.")
      }

      sapply(label_args, function(x) {
        checkmate::assert_data_frame(x)
      })
      sapply(label_args, function(x) {
        checkmate::assert_subset(c("img_value", "label"), names(x))
      })

      self$ggb_labels <- c(self$ggb_labels, label_args)
      return(self)
    },

    render = function() {
      if (is.null(self$ggb_layers) || length(self$ggb_layers) == 0L) {
        stop("No brain layers added to this object yet. Use + geom_brain() to add.")
      }

      img <- self$ggb_images$clone(deep = TRUE)
      img$add_slices(self$ggb_slices)
      do.call(img$add_labels, self$ggb_labels)
      slc <- img$get_slices()

      # image/contrast definitions for each layer
      layer_defs <- trimws(sapply(self$ggb_layers, "[[", "definition"))
      is_contrast <- !layer_defs %in% img$get_image_names() # if definition is just an image name, it's not a contrast to be computed
      layer_sources <- rep(NA_character_, length(layer_defs))

      # data sources for simple image layers
      layer_sources[!is_contrast] <- layer_defs[!is_contrast]

      # compute any contrasts requested
      if (any(is_contrast)) {
        # allow for definitions of the form "con1 := overlay*2"
        contrast_list <- lapply(layer_defs[is_contrast], function(x) {
          if (grepl("^\\s*\\w+\\s*:=.*$", x, perl = TRUE)) {
            con_name <- sub("^\\s*(\\w+)\\s*:=.*$", "\\1", x, perl = TRUE) # parse name
            con_val <- trimws(sub("^\\s*\\w+\\s*:=\\s*(.*)$", "\\1", x, perl = TRUE)) # parse contrast
          } else {
            con_name <- ""
            con_val <- x
          }

          return(c(name = con_name, value = con_val))
        })

        con_names <- sapply(contrast_list, "[[", "name")
        con_names[con_names == ""] <- make.unique(rep("con", sum(con_names == "")))
        contrast_list <- lapply(contrast_list, "[[", "value")
        names(contrast_list) <- con_names

        # fill in layer_sources with appropriate contrast names
        layer_sources[is_contrast] <- con_names

        slc$compute_contrasts(contrast_list)
      }

      # populate source fields in layers so that the appropriate layer can be looked up from slices
      for (ii in seq_along(layer_sources)) {
        self$ggb_layers[[ii]]$source <- layer_sources[ii]
      }

      # need to line up layer names with data name in slice_data
      plot_obj <- ggbrain_plot$new(slc)
      plot_obj$layers <- self$ggb_layers
      plot_obj$generate_plot()

      return(plot_obj$plot())
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
    } else if (o2$action == "add_layers") {
      oc$add_layers(o2$ggb_layers)
    } else if (o2$action == "add_labels") {
      do.call(oc$add_labels, o2$ggb_labels)
    } else if (o2$action == "render") {
      # transform in to patchwork object
      oc <- oc$render()
    }
  }

  return(oc)
}

### Lower-level sugar functions for adding plot elements
### Each returns a ggb object with the relevant action for adding it to the overall ggb object

add_labels <- function(...) {
  args <- list(...)
  ret <- ggb$new(labels=args, action = "add_labels")
}

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
#' @param name the name of this brain layer
#' @param definition a character string of the contrast or image definition used to define this layer.
#'   Can be a simple image name (e.g., 'underlay') or a contrast string (e.g., 'overlay[overlay > 5]')
#' @param color_scale the color scale to be used for this layer (a scale_fill* object)
#' @param show_legend whether to show the color scale for this layer in the legend

#' @details Note that the color_scale and limits must be specified at the time of the geom_contrast creation
#'   in order for them to be mapped properly within ggplot. Because we overlay many raster layers in a ggplot
#'   object that all use the fill aesthetic mapping, it becomes hard to map the color scales after the layer is
#'   created using the typical + scale_fill_X syntax, and similarly for scale limits.
#' @return a ggb object populated with the ggb_layer and the action of 'add_layers'
#' @export
#geom_brain <- function(name=NULL, definition=NULL, ) {
geom_brain <- function(...) {
  inp_args <- list(...)
  l_obj <- do.call(ggbrain_layer$new, inp_args)

  ggb$new(layers = l_obj, action="add_layers")
}

#' Adds the coordinate labels (e.g., x = -6) to each panel
#' @details Note that the data that define the label should be in the ggbrain_slices object that is returned as
#'   part of the add_slices() argument. If you wish to add a custom label, pass it through the add_slices(coord_label)
#'   argument.
annotate_panels <- function(..., position="lower_right") {



}

#' Function to convert ggb object to ggplot/patchwork object
#' @export
render <- function() {
  ggb$new(action = "render")
}

#' little helper function to create named list from objects
#' @param ... A set of arguments to be compiled into a list
#' @details The names of the objects will form the names of the list elements
named_list <- function(...) {
  vnames <- as.character(match.call())[-1]
  return(setNames(list(...), vnames))
}


#' scale for plotting separate color gradients for positive and negative values
#' @param name the scale name to be printed in the legend (above positive scale)
#' @param neg_scale a scale_fill_* object used for negative values
#' @param pos_scale a scale_fill_* object used for positive values
#' @param symmetric if TRUE, the limits of the positive scale will equal the inverse limits of
#'   the negative scale. Said differently, this makes the positive and negative scales symmetric
#' @details Note that this will absolutely not work as a general purpose ggplot2 scale!
#'   The positive/negative combination is achieved by adding two layers/geoms behind the
#'   scenes with different color scale.
#' @importFrom ggplot2 waiver
#' @export
scale_fill_bisided <- function(
  name = waiver(),
  neg_scale = scale_fill_distiller(palette="Blues", direction = 1),
  pos_scale = scale_fill_distiller(palette="Reds"), symmetric = TRUE) {

  checkmate::assert_class(neg_scale, "ScaleContinuous")
  stopifnot(neg_scale$aesthetics == "fill")

  checkmate::assert_class(pos_scale, "ScaleContinuous")
  stopifnot(pos_scale$aesthetics == "fill")

  checkmate::assert_logical(symmetric, len=1L)

  # fake the right class to allow this to validate at the ggbrain_layer stage
  ret <- list(
    name = name,
    neg_scale = neg_scale,
    pos_scale = pos_scale,
    aesthetics = "fill",
    na.value = "transparent",
    symmetric = symmetric
  )

  # hack package into tolerating made up scale object
  class(ret) <- c("list", "ScaleContinuous", "ScaleBisided", "Scale") # ggproto and gg classes lead to errors in print method
  return(ret)
}
