# R6 class for a single layer of a ggbrain panel using outline geom

R6 class for a single layer of a ggbrain panel using outline geom

R6 class for a single layer of a ggbrain panel using outline geom

## Value

a `ggbrain_layer_outline` R6 class with fields related to a brain visual
layer (relates to `geom_outline`)

## Details

Note that this class is exported only for power users and rarely needs
to be called directly in typical use of the package. Instead, look at
[`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md).

## Super class

[`ggbrain::ggbrain_layer`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.md)
-\> `ggbrain_layer_outline`

## Active bindings

- `mapping`:

  the ggplot2 aesthetic mapping between the data columns and the display

- `outline`:

  controls color of outline draw around non-NA (valid) voxels

- `outline_scale`:

  a scale_fill\_\* object containing the ggplot2 outline color scale for
  this layer

- `size`:

  controls size of outline drawn around non-NA (valid) voxels

- `dil_ero`:

  controls the number of pixels to dilate (\> 0) or erode (\< 0) the
  outline

## Methods

### Public methods

- [`ggbrain_layer_outline$new()`](#method-ggbrain_layer_outline-new)

- [`ggbrain_layer_outline$clone()`](#method-ggbrain_layer_outline-clone)

Inherited methods

- [`ggbrain::ggbrain_layer$add_to_gg()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-add_to_gg)
- [`ggbrain::ggbrain_layer$get_data()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-get_data)
- [`ggbrain::ggbrain_layer$is_empty()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-is_empty)
- [`ggbrain::ggbrain_layer$plot()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-plot)
- [`ggbrain::ggbrain_layer$set_breaks()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_breaks)
- [`ggbrain::ggbrain_layer$set_limits()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_limits)
- [`ggbrain::ggbrain_layer$set_neg_breaks()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_neg_breaks)
- [`ggbrain::ggbrain_layer$set_neg_limits()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_neg_limits)
- [`ggbrain::ggbrain_layer$set_pos_breaks()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_pos_breaks)
- [`ggbrain::ggbrain_layer$set_pos_limits()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain_layer.html#method-set_pos_limits)

------------------------------------------------------------------------

### Method `new()`

create a new ggbrain_layer object

#### Usage

    ggbrain_layer_outline$new(
      name = NULL,
      definition = NULL,
      limits = NULL,
      breaks = integer_breaks(),
      show_legend = TRUE,
      interpolate = NULL,
      unify_scales = TRUE,
      alpha = NULL,
      mapping = ggplot2::aes(outline = NULL, fill = NULL),
      outline = NULL,
      outline_scale = NULL,
      size = NULL,
      blur_edge = NULL,
      fill_holes = NULL,
      remove_specks = NULL,
      trim_threads = NULL,
      dil_ero = NULL,
      data = NULL
    )

#### Arguments

- `name`:

  the name of this layer, used for referencing in layer and panel
  modifications

- `definition`:

  an optional character string defining the image or contrast that
  should be used to lookup data from a ggbrain_slices object. This is
  mostly used internally by the ggbrain + syntax to allow layers to be
  defined without data in advance of the plot.

- `limits`:

  if provided, sets the upper and lower bounds on the scale

- `breaks`:

  if provided, a function to draw the breaks on the color scale

- `show_legend`:

  if TRUE, show the scale on the plot legend

- `interpolate`:

  passes to geom_raster and controls whether the fill is interpolated
  over continuous space

- `unify_scales`:

  if TRUE, when this layer is reused across panels, unify the scales to
  match

- `alpha`:

  a number between 0 and 1 that sets the alpha transparency of this
  layer. Default: 1

- `mapping`:

  the aesthetic mapping of the layer data to the display. Should be an
  aes() object and supports `outline` (color of outline around
  clusters). Default is `aes(outline=value)`, which maps the numeric
  value of the layer data to the outline color of the squares at around
  spatial regions. For labeled data, you might use
  `aes(fill=<label_col_name>)`.

- `outline`:

  A character string indicating the color used to outline all non-NA
  pixels in this layer. This is used in distinction to
  `mapping=aes(outline=<variable>)`.

- `outline_scale`:

  a ggplot scale object used for mapping the value column as the outline
  color for the layer.

- `size`:

  controls the thickness of outlines

- `blur_edge`:

  the standard deviation (sigma) of a Gaussian kernel applied to the
  edge of this layer to smooth it. This makes the layer less jagged in
  appearance and is akin to antialiasing.

- `fill_holes`:

  the size of holes (in pixels) inside clusters to be filled by nearest
  neighbor imputation prior to display

- `remove_specks`:

  the size of specks (in pixels) to be removed from each slice prior to
  display

- `trim_threads`:

  the minimum number of neighboring pixels (including diagonals) that
  must be present to keep a pixel

- `dil_ero`:

  the number of pixels to dilate (\> 0) or erode (\<0) the outline.

- `data`:

  the data.frame containing image data for this layer. Must contain
  "dim1", "dim2", and "value" as columns

#### Details

To set mapping, you must provide a ggplot2 aes() object. A
geom_outline() layer requires an `outline` aesthetic mapping, which
controls the color of outlines drawn around regions.

note that the ggbrain_layer_outline class maps onto \*\_fill fields

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggbrain_layer_outline$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
