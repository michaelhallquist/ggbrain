# Sugar functions for making the ggbrain() + layers approach work

#' create ggb container object for a given plot
#' @param images a character vector or existing ggbrain_images object defining which
#'   images should be included in this plot
#' @param labels a list of data.frames for labels?
#' @param slices a set of slices to be added to the plot
#' @param bg_color The background color of the overall plot
#' @param text_color The default text color of the overall plot (passes through to panels)
#' @export
ggbrain <- function(images = NULL, labels = NULL, slices = NULL, title = NULL, bg_color="grey8", text_color="grey92", base_size = 14) {
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

  ggb_obj <- ggb$new(images = img_obj, title=title, bg_color=bg_color, text_color=text_color, base_size=base_size)

  # get_slices = function(slices, img_names = NULL, contrasts = NULL, make_square = TRUE, remove_null_space = TRUE) {
  return(ggb_obj)
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

#' helper function for adding a slice to the plot, allowing for additional panel attributes to be passed through
#' @details I'm still unclear about whether it would be better to unify with this annotate_panels in some way.
add_slice <- function(coordinate = NULL, title = NULL, bg_color = NULL, text_color = NULL, border_color = NULL,
  border_size = NULL, xlab = NULL, ylab = NULL, theme_custom = NULL) {

  checkmate::assert_string(coordinate)

  # store as single-element list with named list inside
  slices <- list(named_list(coordinate, title, bg_color, text_color, border_color, border_size, xlab, ylab, theme_custom))
  ret <- ggb$new(slices = slices, action = "add_slices")
  return(ret)
}

#' Convenience function to add many slices in a montage along one of the 3D planes
#' @param plane a character string specifying the 3D plane: "sagittal", "axial", "coronal", "x", "y", or "z"
#' @param n number of slices to add in this plane. Default: 12
#' @param min the lowest quantile to be included in the montage (between 0 and 1). Default: 0.1
#' @param max the highest quantilt to be included in the montage (between 0 and 1). Default: 0.9
#' @details this can be used with `add_slices` to make a quick montage, such as `add_slices(montage("axial", 10)`.
#' @importFrom dplyr recode
#' @importFrom checkmate assert_integerish assert_subset assert_string assert_number
#' @export
montage <- function(plane = NULL, n = 12, min = 0.1, max = 0.9) {
  checkmate::assert_string(plane)
  plane <- tolower(plane)
  checkmate::assert_subset(plane, c("sagittal", "axial", "coronal", "x", "y", "z"))
  plane <- dplyr::recode(plane, sagittal = "x", coronal = "y", axial = "z") # convert to consistent x, y, z
  checkmate::assert_integerish(n, lower = 1, upper = 1e5)
  checkmate::assert_number(min, lower=0, upper=1)
  checkmate::assert_number(max, lower = 0, upper = 1)

  paste0(plane, " = ", round(seq(min, max, length.out = n) * 100, 3), "%")
}

#' Add images to a ggb object
#' @param images a character vector or ggbrain_images object containing NIfTI images to add to this plot
#' @param fill_holes if TRUE, fill in holes on a slice [WIP]
#' @param clean_specks if TRUE, clean small specks on a slice
#' @param labels a data.frame or named list of data.frame objects corresponding to images that should be labeled.
#'   You can only provide a data.frame if there is a single image being added. If multiple images are added, the names of
#'   the \code{labels} list are used to align the labels with a given matching image.
#' @param filter a named list or character string specifying an expression of values to retain in the image,
#'   or a numeric vector of values to retain. Calls ggbrain_images$filter_image()
#' @return a ggb object with the relevant images and an action of 'add_images'
#' @export
add_images <- function(images = NULL, fill_holes = FALSE, clean_specks = FALSE, labels = NULL, filter=NULL) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
    if (!is.null(labels)) img_obj$add_labels(labels)
  } else {
    img_obj <- ggbrain_images$new(images, fill_holes, clean_specks, labels, filter)
  }

  ret <- ggb$new(images = img_obj, action = "add_images")

  return(ret)
}

#' Helper function to add a contrast layer to the plot. This is a thin wrapper around ggbrain_layer
#' @param name the name of this brain layer
#' @param definition a character string of the contrast or image definition used to define this layer.
#'   Can be a simple image name (e.g., 'underlay') or a contrast string (e.g., 'overlay[overlay > 5]')
#' @param fill_scale the color scale to be used for this layer (a scale_fill* object)
#' @param show_legend whether to show the color scale for this layer in the legend

#' @details Note that the fill_scale and limits must be specified at the time of the geom_contrast creation
#'   in order for them to be mapped properly within ggplot. Because we overlay many raster layers in a ggplot
#'   object that all use the fill aesthetic mapping, it becomes hard to map the color scales after the layer is
#'   created using the typical + scale_fill_X syntax, and similarly for scale limits.
#' @return a ggb object populated with the ggb_layer and the action of 'add_layers'
#' @export
#geom_brain <- function(name=NULL, definition=NULL, ) {
geom_brain <- function(...) {
  # inp_args <- list(...)
  # l_obj <- do.call(ggbrain_layer$new, inp_args)
  l_obj <- ggbrain_layer_brain$new(...)

  ggb$new(layers = l_obj, action="add_layers")
}

#' Variant of geom_brain that only draws outlines, but does not fill them with stats
#' @export
geom_outline <- function(...) {
  # inp_args <- list(...)
  # if (!"mapping" %in% names(inp_args)) {
  #   inp_args$mapping <- ggplot2::aes(fill=NULL, outline=NULL)
  # }
  # l_obj <- do.call(ggbrain_layer$new, inp_args)
  l_obj <- ggbrain_layer_outline$new(...)
  #l_obj$outline_only <- TRUE

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
#' @keywords internal
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
  name = ggplot2::waiver(),
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
