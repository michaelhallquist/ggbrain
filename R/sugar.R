# Sugar functions for making the ggbrain() + layers approach work

#' create ggb container object for a given plot
#' @param images a character vector or existing ggbrain_images object defining which
#'   images should be included in this plot
#' @param labels a list of data.frames for labels?
#' @param slices a set of slices to be added to the plot
#' @param bg_color The background color of the overall plot
#' @param text_color The default text color of the overall plot (passes through to panels)
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

    #' @field ggb_image_labels a named list of data.frames that label corresponding images
    ggb_image_labels = NULL,

    #' @field ggb_slices character string of slices to extract
    ggb_slices = NULL,

    #' @field ggb_layers a list of ggbrain_layer objects containing the bottom-to-top layers to be plotted
    ggb_layers = NULL,

    #' @field ggb_plot a ggbrain_plot object containing the specification of the plot
    ggb_plot = NULL,

    #' @field ggb_annotations a list of annotation objects
    ggb_annotations = NULL,

    #' @field ggb_region_labels a list of ggbrain_label objects to be added as text to label regions
    ggb_region_labels = NULL,

    #' @field action what should this ggb object contribute to another when added with it?
    action = NULL,

    #' @description create a new ggb object. Note that inputs are always cloned to avoid
    #'   unintended modify-in-place behaviors of R6 classes.
    #' @param images a ggbrain_images object containing relevant images
    #' @param slices a character vector of slices to extract
    #' @param layers a list of ggbrain_layer objects
    #' @param labels a list of data.frames with labels that align with one or more images
    #' @param annotations a list of data.frames with annotations that will be added to specific slices
    #' @param region_labels a list of ggbrain_label objects with text-based labels to be drawn on the plot
    #' @param action the action to be taken when adding this object to an existing ggb
    initialize=function(images=NULL, slices=NULL, layers = NULL, labels = NULL, annotations=NULL, region_labels = NULL,
      action=NULL, bg_color="grey50", text_color="black") {
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
        do.call(self$add_image_labels, labels)
      }

      if (!is.null(annotations)) {
        self$add_annotations(annotations)
      }

      if (!is.null(region_labels)) {
        self$add_region_labels(region_labels)
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

    #' @description add annotations to panels
    #' @param annotations a list or data.frame containing the annotations to add to each panel. Minimally,
    #'   the list or data.frame must contain \code{position} and \code{label} columns that define the position
    #'   and text to be added. Other arguments that pass through to ggplot2::annotate() can be provided as columns/elements
    #'   in \code{annotations} and these will be passed through to annotate
    add_annotations = function(annotations = NULL) {
      if (is.null(annotations)) {
        return(self) # no change
      }

      # wrap data.frame input as single-element list for type consistency
      if (checkmate::test_data_frame(annotations)) annotations <- list(annotations)

      checkmate::assert_list(annotations)

      # convert each element to a tibble
      ann <- lapply(annotations, function(aa) {
        checkmate::assert_multi_class(aa, c("data.frame", "list"))

        if (!rlang::has_name(aa, "geom")) aa$geom <- "text" # default to text geom
        if (aa$geom %in% c("label", "text")) {
          # enforce that label and position are present for text/label
          checkmate::assert_subset(c("label", "x", "y"), names(aa))
        }
        tibble::as_tibble(aa) # convert lists to tibbles so that singular values in one field are replicated if other fields are many
      })

      # append annotations
      self$ggb_annotations <- c(self$ggb_annotations, ann)
    },

    #' @description add labels to a given image
    #' @param labels a named list of data.frame objects where the names denote corresponding images
    add_image_labels = function(...) {
      label_args <- list(...)

      # return unchanged object if no input labels found
      if (is.null(label_args) || length(label_args) == 0L) {
        return(self)
      }

      label_names <- names(label_args)
      if (is.null(label_names) || any(label_names == "")) {
        stop("All arguments must be named, with the name referring to the image to be labeled.")
      }

      sapply(label_args, function(x) checkmate::assert_data_frame(x) )
      sapply(label_args, function(x) checkmate::assert_subset(c("value"), names(x)))

      self$ggb_image_labels <- c(self$ggb_image_labels, label_args)
      return(self)
    },

    #' @description add a list of ggbrain_label objects to the overall ggb for compiling a plot
    #' @param labels a list of data.frames with region labels that should be plotted on each slice. This is generated
    #'   internally by ggbrain_images$get_slices() in the $slice_labels field.
    add_region_labels = function(labels = NULL) {
      if (checkmate::test_class(labels, "ggbrain_label")) {
        labels <- list(labels) # make into one-element list for consistency
      }

      sapply(labels, function(x) checkmate::assert_class(x, "ggbrain_label"))
      self$ggb_region_labels <- c(self$ggb_region_labels, labels)
    },

    #' @description this method converts the ggb object into a compiled ggplot2 object that can then be passed to other
    #'   functions from cowplot, ggplot2, and patchwork. Once the object is rendered, it no longer retains the underlying ggb
    #'   fields that contain the elemental data.
    render = function() {
      if (is.null(self$ggb_layers) || length(self$ggb_layers) == 0L) {
        stop("No brain layers added to this object yet. Use + geom_brain() to add.")
      }

      img <- self$ggb_images$clone(deep = TRUE)
      img$add_slices(self$ggb_slices)

      if (!is.null(self$ggb_image_labels))  do.call(img$add_labels, self$ggb_image_labels)
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
      plot_obj$annotations <- self$ggb_annotations # pass through annotations
      plot_obj$region_labels <- self$ggb_region_labels # pass through region labels
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
    } else if (o2$action == "add_image_labels") {
      do.call(oc$add_image_labels, o2$ggb_image_labels)
    } else if (o2$action == "add_annotations") {
      oc$add_annotations(o2$ggb_annotations)
    } else if (o2$action == "add_region_labels") {
      oc$add_region_labels(o2$ggb_region_labels)
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
  ggb$new(labels=args, action = "add_image_labels")
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
#' @param fill_holes if TRUE, fill in holes on a slice [WIP]
#' @param clean_specks if TRUE, clean small specks on a slice [WIP]
#' @param labels a data.frame or named list of data.frame objects corresponding to images that should be labeled.
#'   You can only provide a data.frame if there is a single image being added. If multiple images are added, the names of
#'   the \code{labels} list are used to align the labels with a given matching image.
#' @return a ggb object with the relevant images and an action of 'add_images'
#' @export
add_images <- function(images = NULL, fill_holes = FALSE, clean_specks = FALSE, labels = NULL) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
    if (!is.null(labels)) img_obj$add_labels(labels)
  } else {
    img_obj <- ggbrain_images$new(images, fill_holes, clean_specks, labels)
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
  #inp_args <- list(...)
  #l_obj <- do.call(ggbrain_layer$new, inp_args)
  l_obj <- ggbrain_layer$new(...)

  ggb$new(layers = l_obj, action="add_layers")
}

#' Variant of geom_brain that only draws outlines, but does not fill them with stats
#' @export
geom_outline <- function(...) {
  #inp_args <- list(...)
  #l_obj <- do.call(ggbrain_layer$new, inp_args)
  l_obj <- ggbrain_layer$new(...)
  l_obj$outline_only <- TRUE

  ggb$new(layers = l_obj, action = "add_layers")
}

#' Variant of geom_text used for plotting region labels on slices
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param ... All other parameters passed through to geom_text
#' @export
geom_region_text <- function(image, label_column = "label", ...) {
  l_obj <- ggbrain_label$new(geom = "text", image = image, label_column = label_column, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Variant of geom_label used for plotting region labels on slices
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param ... All other parameters passed through to geom_label
#' @export
geom_region_label <- function(image, label_column = "label", ...) {
  l_obj <- ggbrain_label$new(geom = "label", image = image, label_column = label_column, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Variant of geom_text_repel used for plotting region labels on slices with separation from other labels
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param ... All other parameters passed through to geom_text_repel
#' @export
geom_region_text_repel <- function(image, label_column = "label", ...) {
  l_obj <- ggbrain_label$new(geom = "text_repel", image = image, label_column = label_column, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}


#' Variant of geom_label_repel used for plotting region labels on slices with separation from other labels
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param ... All other parameters passed through to geom_label_repel
#' @export
geom_region_label_repel <- function(image, label_column = "label", ...) {
  l_obj <- ggbrain_label$new(geom = "label_repel", image = image, label_column = label_column, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Adds custom annotations to a single panel on the ggbrain plot
#' @param x the x position of the annotation. If numeric, it is assumed to be the pixel position along the x axis (e.g., 26).
#'   In addition, convenience values of 'left', 'right', or 'q[1-100]' can be used to look up the left-most, right-most, or quantile-based
#'   positions along the x axis.
#' @param y the y position of the annotation. If numeric, it is assumed to be the pixel position along the y axis (e.g., 26).
#'   In addition, convenience values of 'left', 'right', or 'q[1-100]' can be used to look up the left-most, right-most, or quantile-based
#'   positions along the x axis.
#' @param slice_index the slice number to which this annotation is added. These are numbered in the wrapping order from 
#'   patchwork::wrap_plots, which will normally go from top-left to bottom-right.
#' @details Note that this only handles a single annotation on a single panel! If you want to annotate en masse, use annotate_panels
#'   with a data.frame where each row is an annotation.
#' @export
annotate_panel <- function(x = "middle", y = "middle", slice_index=NULL, ...) {
  checkmate::assert_number(slice_index, lower=1)
  ggb$new(annotations = list(list(x = x, y = y, slice_index=slice_index, ...)), action = "add_annotations")
}

#' Adds custom annotations to one or more panels on the ggbrain plot
#' @param panel_data a list or data.frame containing annotations to be added
#' @export
annotate_panels <- function(panel_data) {

}

#' Adds the coordinate labels to each panel based on the location of the slice along the slicing axis (e.g., z = 15)
#' @param x the x position of the coordinate label. If numeric, it is assumed to be the pixel position along the x axis (e.g., 26).
#'   In addition, convenience values of 'left', 'right', or 'q[1-100]' can be used to look up the left-most, right-most, or quantile-based
#'   positions along the x axis.
#' @param y the y position of the coordinate label. If numeric, it is assumed to be the pixel position along the y axis (e.g., 26).
#'   In addition, convenience values of 'top', 'bottom', or 'q[1-100]' can be used to look up the top-most, bottom-most, or quantile-based
#'   positions along the y axis.
#' @param ... any other arguments to ggplot2::annotate, which will be passed through to each panel
#' @export
annotate_coordinates <- function(x="right", y="bottom", ...) {
  ggb$new(annotations = list(list(label = ".coord_label", x = x, y = y, ...)), action = "add_annotations")
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
