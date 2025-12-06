# Generic R6 base class that is used to support + semantics

Generic R6 base class that is used to support + semantics

Generic R6 base class that is used to support + semantics

## Details

this object becomes a simple storage class that contains all relevant
objects (e.g., ggbrain_images) required to generate a brain plot

## Public fields

- `ggb_images`:

  ggbrain_images object for this plot

- `ggb_image_labels`:

  a named list of data.frames that label corresponding images

- `ggb_slices`:

  list slices to extract for this plot

- `ggb_cluster_slices`:

  a list of cluster_slices_spec objects for deferred slice computation

- `ggb_cluster_data`:

  a list of data.frames containing cluster information from resolved
  cluster_slices

- `ggb_contrasts`:

  a character vector of contrasts to be computed as part of this plot

- `ggb_layers`:

  a list of ggbrain_layer objects containing the bottom-to-top layers to
  be plotted

- `ggb_plot`:

  a ggbrain_plot object containing the specification of the plot

- `ggb_annotations`:

  a list of annotation objects

- `ggb_region_labels`:

  a list of ggbrain_label objects to be added as text to label regions

- `ggb_target_resolution`:

  a list with target resolution settings (voxel_size, interpolation,
  interpolation_value)

- `action`:

  what should this ggb object contribute to another when added with it?

## Methods

### Public methods

- [`ggb$new()`](#method-ggb-new)

- [`ggb$add_layers()`](#method-ggb-add_layers)

- [`ggb$add_slices()`](#method-ggb-add_slices)

- [`ggb$add_cluster_slices()`](#method-ggb-add_cluster_slices)

- [`ggb$get_cluster_data()`](#method-ggb-get_cluster_data)

- [`ggb$add_contrasts()`](#method-ggb-add_contrasts)

- [`ggb$add_annotations()`](#method-ggb-add_annotations)

- [`ggb$add_image_labels()`](#method-ggb-add_image_labels)

- [`ggb$add_region_labels()`](#method-ggb-add_region_labels)

- [`ggb$render()`](#method-ggb-render)

- [`ggb$get_slice_data()`](#method-ggb-get_slice_data)

- [`ggb$plot()`](#method-ggb-plot)

- [`ggb$clone()`](#method-ggb-clone)

------------------------------------------------------------------------

### Method `new()`

create a new ggb object. Note that inputs are always cloned to avoid
unintended modify-in-place behaviors of R6 classes.

#### Usage

    ggb$new(
      images = NULL,
      slices = NULL,
      contrasts = NULL,
      layers = NULL,
      labels = NULL,
      annotations = NULL,
      region_labels = NULL,
      title = NULL,
      bg_color = NULL,
      text_color = NULL,
      base_size = NULL,
      action = NULL
    )

#### Arguments

- `images`:

  a ggbrain_images object containing relevant images

- `slices`:

  a character vector of slices to extract

- `contrasts`:

  a character vector of contrasts to define and compute

- `layers`:

  a list of ggbrain_layer objects

- `labels`:

  a list of data.frames with labels that align with one or more images

- `annotations`:

  a list of data.frames with annotations that will be added to specific
  slices

- `region_labels`:

  a list of ggbrain_label objects with text-based labels to be drawn on
  the plot

- `title`:

  overall title of the plot

- `bg_color`:

  the background color of the overall plot

- `text_color`:

  the text color of the overall plot; if NULL, a contrasting color is
  chosen from the background

- `base_size`:

  the base size of text on the plot

- `action`:

  the action to be taken when adding this object to an existing ggb

------------------------------------------------------------------------

### Method `add_layers()`

add layers from another ggb object to this one

#### Usage

    ggb$add_layers(ilist)

#### Arguments

- `ilist`:

  a list of ggbrain_layer objects. If a ggb object is passed, we will
  get this list from obj\$ggb_layers

------------------------------------------------------------------------

### Method `add_slices()`

add slices to the existing vector of slices

#### Usage

    ggb$add_slices(slices = NULL)

#### Arguments

- `slices`:

  a character vector of slices to be appended to the existing slices

------------------------------------------------------------------------

### Method `add_cluster_slices()`

add a cluster_slices_spec for deferred slice computation

#### Usage

    ggb$add_cluster_slices(spec = NULL)

#### Arguments

- `spec`:

  a cluster_slices_spec object from cluster_slices()

------------------------------------------------------------------------

### Method `get_cluster_data()`

retrieve cluster data from resolved cluster_slices specifications

#### Usage

    ggb$get_cluster_data()

#### Details

This method returns the cluster information computed during rendering.
It must be called after plot() or render() has been invoked, otherwise
it returns NULL. The returned data.frame contains columns: cluster_id,
size (in voxels), com_i, com_j, com_k (center of mass in voxel
coordinates), com_x, com_y, com_z (center of mass in world/mm
coordinates), and slice_coord (the slice coordinate string used for
plotting).

#### Returns

A data.frame with cluster information, or NULL if no cluster_slices were
used or render() hasn't been called

------------------------------------------------------------------------

### Method `add_contrasts()`

add contrast definitions to the plot object

#### Usage

    ggb$add_contrasts(contrasts)

#### Arguments

- `contrasts`:

  a character vector of contrasts to compute as part of the plot
  generation

------------------------------------------------------------------------

### Method `add_annotations()`

add annotations to panels

#### Usage

    ggb$add_annotations(annotations = NULL)

#### Arguments

- `annotations`:

  a list or data.frame containing the annotations to add to each panel.
  Minimally, the list or data.frame must contain `position` and `label`
  columns that define the position and text to be added. Other arguments
  that pass through to ggplot2::annotate() can be provided as
  columns/elements in `annotations` and these will be passed through to
  annotate

------------------------------------------------------------------------

### Method `add_image_labels()`

add labels to a given image

#### Usage

    ggb$add_image_labels(...)

#### Arguments

- `...`:

  a named list of arguments where each is a data.frame with labels
  denoting corresponding images

------------------------------------------------------------------------

### Method `add_region_labels()`

add a list of ggbrain_label objects to the overall ggb for compiling a
plot

#### Usage

    ggb$add_region_labels(labels = NULL)

#### Arguments

- `labels`:

  a list of data.frames with region labels that should be plotted on
  each slice. This is generated internally by
  ggbrain_images\$get_slices() in the \$slice_labels field.

------------------------------------------------------------------------

### Method [`render()`](https://michaelhallquist.github.io/ggbrain/reference/render.md)

this method converts the ggb object into a compiled ggplot2 object that
can then be passed to other functions from cowplot, ggplot2, and
patchwork. Once the object is rendered, it no longer retains the
underlying ggb fields that contain the elemental data.

#### Usage

    ggb$render(guides = "collect")

#### Arguments

- `guides`:

  Passes through to patchwork::plot_layout to control how legends are
  combined across plots. The default is "collect", which collects
  legends within a given nesting level (removes duplicates).

------------------------------------------------------------------------

### Method `get_slice_data()`

get the slice data from the rendered plot for inspection

#### Usage

    ggb$get_slice_data(image_name = NULL, slice_index = NULL, as_matrix = FALSE)

#### Arguments

- `image_name`:

  optional character string specifying a single image to extract. If
  NULL, returns all images for each slice.

- `slice_index`:

  optional integer vector specifying which slices to return. If NULL,
  returns all slices.

- `as_matrix`:

  if TRUE, convert data.frames to matrices using df2mat(). Default:
  FALSE

#### Details

If
[`render()`](https://michaelhallquist.github.io/ggbrain/reference/render.md)
has not been called yet, this method will call it automatically to
populate the slice data. This is useful for verifying that
resampling/interpolation occurred as expected when using
[`target_resolution()`](https://michaelhallquist.github.io/ggbrain/reference/target_resolution.md).

#### Returns

A list of slice data. If `image_name` is specified, returns a list of
data.frames (or matrices) for that image across slices. If `image_name`
is NULL, returns a nested list where each element contains all images
for that slice.

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

plot this ggb object – just an alias for render

#### Usage

    ggb$plot(guides = "collect")

#### Arguments

- `guides`:

  Passes through to patchwork::plot_layout to control how legends are
  combined across plots. The default is "collect", which collects
  legends within a given nesting level (removes duplicates).

#### Details

requires that required elements are in place already.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggb$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
