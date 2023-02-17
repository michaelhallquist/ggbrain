# Sugar functions for making the ggbrain() + layers approach work

#' create ggb container object for a given plot
#' @param images a character vector or existing ggbrain_images object defining which
#'   images should be included in this plot
#' @param slices a set of slices to be added to the plot
#' @param title the overall title to be added to the plot
#' @param bg_color The background color of the overall plot
#' @param text_color The default text color of the overall plot (passes through to panels)
#' @param base_size The base size of fonts used in the plot (cf. `theme_minimal`)
#' @return a `ggb` object containing basic information for a `ggbrain` plot such as background color,
#'   text color, and font size
#' @export
ggbrain <- function(images = NULL, slices = NULL, title = NULL, bg_color="grey8", text_color="grey92", base_size = 14) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
  } else {
    img_obj <- ggbrain_images$new(images)
  }

  if (!is.null(slices)) {
    img_obj$reset_slices(slices)
    img_obj$add_slices(slices)
  }

  ggb_obj <- ggb$new(images = img_obj, title=title, bg_color=bg_color, text_color=text_color, base_size=base_size)

  return(ggb_obj)
}


### Lower-level sugar functions for adding plot elements
### Each returns a ggb object with the relevant action for adding it to the overall ggb object

add_labels <- function(...) {
  args <- list(...)
  ggb$new(labels = args, action = "add_image_labels")
}

#' Add images to a ggbrain object
#' @param images a character vector or ggbrain_images object containing NIfTI images to add to this plot
#' @param volumes a number indicating the volume within the \code{images} to display. At present, this must
#'   be a single number -- perhaps in the future, it could be a vector so that many timepoints in a 4-D image could
#'   be displayed.
#' @param labels a data.frame or named list of data.frame objects corresponding to images that should be labeled.
#'   You can only provide a data.frame if there is a single image being added. If multiple images are added, the names of
#'   the \code{labels} list are used to align the labels with a given matching image.
#' @param filter a named list or character string specifying an expression of values to retain in the image,
#'   or a numeric vector of values to retain. Calls ggbrain_images$filter_image()
#' @return a `ggb` object with the relevant images and an action of 'add_images'
#' @examples
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1))
#' @export
images <- function(images = NULL, volumes = NULL, labels = NULL, filter = NULL) {
  if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE) # work from copy
    if (!is.null(labels)) img_obj$add_labels(labels)
  } else {
    img_obj <- ggbrain_images$new(images, volumes, labels, filter)
  }

  ret <- ggb$new(images = img_obj, action = "add_images")

  return(ret)
}

#' Adds slices to the ggbrain plot, including additional panel aesthetics
#' @param coordinates a character vector specifying the x, y, or z coordinates of the slices to be added.
#' @param title a title for the slice panels added to the ggplot object using `ggtitle()`
#' @param bg_color the color used for the background of the panels. Default: \code{'gray10'} (nearly black)
#' @param text_color the color used for text displayed on the panels. Default: \code{'white'}.
#' @param border_color the color used for drawing a border around on the panels. Default: \code{'gray50'}
#'   (though borders are not drawn by default).
#' @param border_size the size of the border line drawn around the panels. Default: NULL. If this value is
#'   greater than zero, a border of this size and with color \code{border_color} will be drawn around the panels.
#' @param xlab The label to place on x axis. Default is NULL.
#' @param ylab The label to place on y axis. Default is NULL.
#' @param theme_custom Any custom theme() settings to be added to the panels.
#' @details note that if you pass in multiple coordinates (as a vector), the \code{title}, \code{bg_color}, and other attributes
#'   will be reused for all slices added by this operation. Thus, if you want to customize specific slices or groups of slices, use
#'   multiple addition operations, as in `slices(c('x=10', 'y=15'), bg_color='white') + slices(c('x=18', 'y=22'), bg_color='black')`.
#' @return a `ggb` object with the relevant slices and an action of 'add_slices'
#' @examples
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1)) +
#'     slices(c("x = 25%", "x = 75%"), border_color = "blue")
#' @export
slices <- function(coordinates = NULL, title = NULL, bg_color = NULL, text_color = NULL, border_color = NULL,
                   border_size = NULL, xlab = NULL, ylab = NULL, theme_custom = NULL) {
  checkmate::assert_character(coordinates)

  # store as single-element list with named list inside -- title, bg_color, etc. are shared by each slice added
  slices <- lapply(coordinates, function(coordinate) {
    named_list(coordinate, title, bg_color, text_color, border_color, border_size, xlab, ylab, theme_custom)
  })

  ret <- ggb$new(slices = slices, action = "add_slices")
  return(ret)
}

#' Convenience function to add many slices in a montage along one of the 3D planes
#' @param plane a character string specifying the 3D plane: "sagittal", "axial", "coronal", "x", "y", or "z"
#' @param n number of slices to add in this plane. Default: 12
#' @param min the lowest quantile to be included in the montage (between 0 and 1). Default: 0.1
#' @param max the highest quantile to be included in the montage (between 0 and 1). Default: 0.9
#' @param min_coord the lowest spatial position (in image coordinate space) to be included in the montage.
#' @param max_coord the highest spatial position (in image coordinate space) to be included in the montage.
#' @details
#'   This can be used with `slices` to make a quick montage, such as `slices(montage("axial", 10)`.
#'
#'   Also note that use of standardized coordinates (in quantiles, using `min` and `max`) is mutually exclusive
#'   with the the image coordinate specifications `min_coord` and `max_coord.`
#' @return a character string containing the slice positions along the requested axis
#' @examples
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1)) +
#'     slices(montage("sagittal", 15))
#' @importFrom dplyr recode
#' @importFrom checkmate assert_integerish assert_subset assert_string assert_number
#' @export
montage <- function(plane = NULL, n = 12, min = 0.1, max = 0.9, min_coord = NULL, max_coord = NULL) {
  checkmate::assert_string(plane)
  plane <- tolower(plane)
  checkmate::assert_subset(plane, c("sagittal", "axial", "coronal", "x", "y", "z"))
  plane <- dplyr::recode(plane, sagittal = "x", coronal = "y", axial = "z") # convert to consistent x, y, z
  checkmate::assert_integerish(n, lower = 1, upper = 1e5)
  checkmate::assert_number(min, lower = 0, upper = 1)
  checkmate::assert_number(max, lower = 0, upper = 1)

  if (!is.null(min_coord) && !is.null(max_coord)) {
    checkmate::assert_number(min_coord)
    checkmate::assert_number(max_coord)
    v <- paste0(plane, " = ", round(seq(min_coord, max_coord, length.out = n), 3))
  } else {
    v <- paste0(plane, " = ", round(seq(min, max, length.out = n) * 100, 3), "%")
  }

  return(v)
}

#' Adds contrast definitions to the ggbrain plot
#' @param contrasts a character vector or list containing contrasts to be computed as part of the
#'   ggbrain object definition.
#' @details \code{contrasts} must take the form of `<name> := <value expression>` or must use a named vector.
#'   Note that defining a contrast does not directly impact the appearance of the plot unless the
#'   contrast is named in a geom_* layer.
#'
#'   Also note that contrasts can be specified in the definition of a layer. Thus, the \code{define} function
#'   has two primary virtues. First, it allows for the conceptual separation of contrast definition versus usage
#'   inside a geom_* layer, which is particularly useful if a contrast is used across several layers. Second, it allows
#'   downstream layers to further modify the contrast, such as when we compute a
#'
#' @examples
#'   # T1-weighted template
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#' 
#'   # signed reward prediction error map
#'   signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05.nii.gz", package = "ggbrain")
#' 
#'   # unsigned (absolute value) prediction error map
#'   abspe <- system.file("extdata", "abspe_ptfce_fwep_0.05.nii.gz", package = "ggbrain")
#' 
#'   # simple example of a difference contrast, separating definition from usage in geom_brain
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     define("signed_gt_abs := signed_pe - abspe") +
#'     geom_brain("signed_gt_abs")
#' 
#'   # you can also use a named vector in define(), which is equivalent
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     define(c(signed_gt_abs = "signed_pe - abspe")) +
#'     geom_brain("signed_gt_abs")
#'     
#'   # contrast definitions can also occur inline, yielding equivalent plots
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     geom_brain("signed_pe - abspe")
#'     
#'   # The use of contrasts() is helpful when layers modify the contrast (e.g., subsetting)
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     define(c(signed_gt_abs = "signed_pe - abspe")) +
#'     geom_brain(
#'       "signed_gt_abs[signed_gt_abs > 0]", 
#'       fill_scale=ggplot2::scale_fill_distiller("Pos diff", palette = "Reds")
#'     )
#' @return a `ggb` object with the relevant contrasts and an action of 'add_contrasts'
#' @export
define <- function(contrasts = NULL) {
  checkmate::assert_character(contrasts)

  ret <- ggb$new(contrasts = contrasts, action = "add_contrasts")
  return(ret)
}


#' Adds a raster layer to the ggbrain plot, displaying pixels from the specified layer definition
#'
#' @param definition a character string of the contrast or image definition used to define this layer.
#'   Can be a simple image name (e.g., 'underlay') or a contrast string (e.g., \code{'overlay[overlay > 5]'})
#' @param name the name of this layer, used for referencing in layer and panel modifications
#' @param fill A character string indicating the color used to fill all non-NA pixels in this layer. This is used to set
#'   the fill color, in distinction to color mapping: \code{mapping=aes(fill=<variable>)}.
#' @param fill_scale a ggplot scale_fill_* object used for mapping the fill column to the color of pixels in this layer.
#' @param mapping the aesthetic mapping of the layer data to the display. Should be an aes() object and supports
#'   `fill` (color of filled pixels). Default is `aes(fill=value)`, which maps the numeric value of the layer data
#'   to the fill color of the squares at each spatial position. For labeled data, you might use \code{aes(fill=<label_col_name>)}.
#' @param limits if provided, sets the upper and lower bounds on the scale
#' @param breaks if provided, a function to draw the breaks on the fill scale
#' @param show_legend if TRUE, show the fill scale in the plot legend
#' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
#' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
#' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
#' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
#'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
#' @param fill_holes An optional positive integer specifying the size of holes (in pixels) inside clusters
#'   to be filled by nearest neighbor imputation. Default: 0.
#' @param remove_specks An optional positive integer specifying the size of specks (in pixels) to be removed from each slice prior
#'   to display. Specks are small clusters that may be distracting and contribute to a 'salt and pepper' appearance.
#' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel.
#'
#' @details Note that the fill_scale and limits must be specified at the time of the geom_brain creation
#'   in order for them to be mapped properly within ggplot. Because we overlay many raster layers in a ggplot
#'   object that all use the fill aesthetic mapping, it becomes hard to map the color scales after the layer is
#'   created using the typical + scale_fill_* syntax, and similarly for scale limits.
#' @return a ggb object populated with the relevant geom_brainand the action of 'add_layers'
#' @examples
#'   # T1-weighted template
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#' 
#'   # signed reward prediction error map
#'   signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05.nii.gz", package = "ggbrain")
#'   
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, overlay = signed_pe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     geom_brain("underlay") +
#'     geom_brain(definition="overlay[overlay > 1]", fill_scale=ggplot2::scale_fill_viridis_c("pos z"))
#' @export
geom_brain <- function(definition = NULL, name = NULL, fill = NULL, fill_scale = NULL, mapping = NULL,
      limits = NULL, breaks = NULL, show_legend = TRUE, interpolate = FALSE, unify_scales=TRUE, alpha = NULL,
      blur_edge = NULL, fill_holes = NULL, remove_specks = NULL, trim_threads = NULL) {

  arglist <- named_list(definition, name, limits, breaks, show_legend, interpolate, unify_scales,
                  alpha, mapping, fill, fill_scale, blur_edge, fill_holes, remove_specks, trim_threads)

  # only pass through non-NULLs so that default arguments of layer are used when no input is provided
  arglist <- arglist[!sapply(arglist, is.null)]

  # this generates problems because it overrides default arguments of layer class
  # l_obj <- ggbrain_layer_brain$new(
  #   name, definition, limits, breaks, show_legend, interpolate, unify_scales,
  #   alpha, mapping, fill, fill_scale, blur_edge, fill_holes, remove_specks, trim_threads
  # )

  l_obj <- do.call(ggbrain_layer_brain$new, arglist)

  ggb$new(layers = l_obj, action="add_layers")
}


#' Adds an outline layer to the ggbrain plot, displaying outlines from the non-missing pixels in the specified layer definition
#'
#' @param definition a character string of the contrast or image definition used to define this layer.
#'   Can be a simple image name (e.g., 'underlay') or a contrast string (e.g., \code{'overlay[overlay > 5]'})
#' @param name the name of this layer, used for referencing in layer and panel modifications
#' @param outline A character string indicating the color used to draw outlines in this layer. This is used to set
#'   the outline color, in distinction to outline color mapping: \code{mapping=aes(outline=<variable>)}.
#' @param outline_scale a ggplot scale_fill_* object used for mapping the fill column to the color of pixels in this layer.
#' @param mapping the aesthetic mapping of the layer data to the display. Should be an aes() object and supports
#'   `outline` (outline color of pixels). Default is `aes(outline=NULL)`, which uses a set outline color.
#' @param size the size of outlines to be drawn in pixel units. Default: 1
#' @param limits if provided, sets the upper and lower bounds on the scale
#' @param breaks if provided, a function to draw the breaks on the fill scale
#' @param show_legend if TRUE, show the fill scale in the plot legend
#' @param interpolate passes to geom_raster and controls whether the fill is interpolated over continuous space
#' @param unify_scales if TRUE, when this layer is reused across panels, unify the scales to match
#' @param alpha a number between 0 and 1 that sets the alpha transparency of this layer. Default: 1
#' @param blur_edge the standard deviation (sigma) of a Gaussian kernel applied to the edge of this layer to
#'   smooth it. This makes the layer less jagged in appearance and is akin to antialiasing.
#' @param fill_holes An optional positive integer specifying the size of holes (in pixels) inside clusters
#'   to be filled by nearest neighbor imputation. Default: 0.
#' @param remove_specks An optional positive integer specifying the size of specks (in pixels) to be removed from each slice prior
#'   to display. Specks are small clusters that may be distracting and contribute to a 'salt and pepper' appearance.
#' @param trim_threads the minimum number of neighboring pixels (including diagonals) that must be present to keep a pixel.
#' @param dil_ero the number of pixels to dilate (> 0) or erode (< 0) the outline for display purposes. Default: 0L
#'
#' @details Note that the fill_scale and limits must be specified at the time of the geom_brain creation
#'   in order for them to be mapped properly within ggplot. Because we overlay many raster layers in a ggplot
#'   object that all use the fill aesthetic mapping, it becomes hard to map the color scales after the layer is
#'   created using the typical + scale_fill_* syntax, and similarly for scale limits.
#' @return a ggb object populated with the geom_outline layer and the action of 'add_layers'
#' @examples
#'   # T1-weighted template
#'   t1 <- system.file("extdata", "mni_template_2009c_3mm.nii.gz", package = "ggbrain")
#' 
#'   # signed reward prediction error map
#'   signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05.nii.gz", package = "ggbrain")
#'   
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, overlay = signed_pe)) +
#'     slices(c("x = 25%", "x = 75%")) +
#'     geom_brain("underlay") +
#'     geom_outline(definition="overlay[overlay > 2]", outline="cyan")
#' @export
geom_outline <- function(definition = NULL, name = NULL, outline = NULL, outline_scale = NULL, 
      mapping = ggplot2::aes(outline = NULL, fill=NULL), size = NULL, limits = NULL, breaks = integer_breaks(), 
      show_legend = TRUE, interpolate = FALSE, unify_scales=TRUE, alpha = 1.0,
      blur_edge = NULL, fill_holes = NULL, remove_specks = NULL, trim_threads = NULL, dil_ero = 0L) {
  
  arglist <- named_list(definition, name, limits, breaks, show_legend, interpolate, unify_scales,
                        alpha, mapping, outline, outline_scale, size, blur_edge, fill_holes, remove_specks, trim_threads, dil_ero)
  
  # only pass through non-NULLs so that default arguments of layer are used when no input is provided
  arglist <- arglist[!sapply(arglist, is.null)]
  
  # this generates problems because it overrides default arguments of layer class
  # l_obj <- ggbrain_layer_outline$new(
  #   name, definition, limits, breaks, show_legend, interpolate, unify_scales,
  #   alpha, mapping, outline, outline_scale, size, blur_edge, fill_holes, remove_specks, trim_threads
  # )
  
  l_obj <- do.call(ggbrain_layer_outline$new, arglist)

  ggb$new(layers = l_obj, action = "add_layers")
}

#' Variant of geom_text used for plotting region labels on slices
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param min_px The minimum number of pixels present on a slice that will result in a text label. Default: 1
#' @param ... All other parameters passed through to geom_text
#' @return a `ggb` object with the relevant ggbrain_label field and an action of "add_region_labels"
#' @export
geom_region_text <- function(image, label_column = "label", min_px=1L, ...) {
  l_obj <- ggbrain_label$new(geom = "text", image = image, label_column = label_column, min_px=min_px, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Variant of geom_label used for plotting region labels on slices
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param min_px The minimum number of pixels present on a slice that will result in a text label. Default: 1
#' @param ... All other parameters passed through to geom_label
#' @return a `ggb` object with the relevant ggbrain_label field and an action of "add_region_labels"
#' @export
geom_region_label <- function(image, label_column = "label", min_px=1L, ...) {
  l_obj <- ggbrain_label$new(geom = "label", image = image, label_column = label_column, min_px=min_px, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Variant of geom_text_repel used for plotting region labels on slices with separation from other labels
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param min_px The minimum number of pixels present on a slice that will result in a text label. Default: 1
#' @param ... All other parameters passed through to geom_text_repel
#' @return a `ggb` object with the relevant ggbrain_label field and an action of "add_region_labels"
#' @export
geom_region_text_repel <- function(image, label_column = "label", min_px=1L, ...) {
  l_obj <- ggbrain_label$new(geom = "text_repel", image = image, label_column = label_column, min_px=min_px, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Variant of geom_label_repel used for plotting region labels on slices with separation from other labels
#' @param image The name of the image within the underlying ggbrain_slices object that contains the labeled data positions
#' @param label_column The column name name for the labels to use within the slice data
#' @param min_px The minimum number of pixels present on a slice that will result in a text label. Default: 1
#' @param ... All other parameters passed through to geom_label_repel
#' @return a `ggb` object with the relevant ggbrain_label field and an action of "add_region_labels"
#' @export
geom_region_label_repel <- function(image, label_column = "label", min_px=1L, ...) {
  l_obj <- ggbrain_label$new(geom = "label_repel", image = image, label_column = label_column, min_px=min_px, ...)
  ggb$new(region_labels = l_obj, action = "add_region_labels")
}

#' Adds custom annotations to a single panel on the ggbrain plot
#' @param x the x position of the annotation. If numeric, it is assumed to be the pixel position along the x axis (e.g., 26).
#'   In addition, convenience values of 'left', 'right', or \code{'q[1-100]'} can be used to look up the left-most, right-most, or quantile-based
#'   positions along the x axis.
#' @param y the y position of the annotation. If numeric, it is assumed to be the pixel position along the y axis (e.g., 26).
#'   In addition, convenience values of 'left', 'right', or \code{'q[1-100]'} can be used to look up the left-most, right-most, or quantile-based
#'   positions along the x axis.
#' @param slice_index the slice number to which this annotation is added. These are numbered in the wrapping order from
#'   patchwork::wrap_plots, which will normally go from top-left to bottom-right.
#' @param ... Additional parameters passed to ggplot2::annotate such as \code{label} or \code{geom}
#' @details Note that this only handles a single annotation on a single panel!
#' @return a `ggb` object with the relevant annotations field and an action of "add_annotations"
#' @export
annotate_panel <- function(x = "middle", y = "middle", slice_index=NULL, ...) {
  checkmate::assert_number(slice_index, lower=1)
  ggb$new(annotations = list(list(x = x, y = y, slice_index=slice_index, ...)), action = "add_annotations")
}

#' Adds the coordinate labels to each panel based on the location of the slice along the slicing axis (e.g., z = 15)
#' @param x the x position of the coordinate label. If numeric, it is assumed to be the pixel position along the x axis (e.g., 26).
#'   In addition, convenience values of \code{"left"}, \code{"right"}, or \code{"q[1-100]"} can be used to look up the left-most,
#'   right-most, or quantile-based positions along the x axis.
#' @param y the y position of the coordinate label. If numeric, it is assumed to be the pixel position along the y axis (e.g., 26).
#'   In addition, convenience values of 'top', \code{"bottom"}, or \code{"q[1-100]"} can be used to look up the top-most, bottom-most,
#'   or quantile-based positions along the y axis.
#' @param ... any other arguments to ggplot2::annotate, which will be passed through to each panel
#' @return a `ggb` object with the action 'add_annotations', used in a `ggbrain` addition chain
#' @export
annotate_coordinates <- function(x="right", y="bottom", ...) {
  ggb$new(annotations = list(list(label = ".coord_label", x = x, y = y, ...)), action = "add_annotations")
}

#' Function to convert `ggb` object to ggplot/patchwork object
#' @return a `ggb` object with the action 'render', used in a `ggbrain` addition chain
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
#' @return a `ggplot2` scale of type `ScaleContinuous` that includes negative and positive fill
#'   scales internally in the `$neg_scale` and `$pos_scale` elements
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
