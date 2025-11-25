# R6 class for a single layer of a ggbrain panel

R6 class for a single layer of a ggbrain panel

R6 class for a single layer of a ggbrain panel

## Value

a `ggbrain_layer` R6 class containing fields related to a visual layer
on the `ggbrain` plot

## Details

Note that this class is exported only for power users and rarely needs
to be called directly in typical use of the package. Instead, look at
geom_brain() and geom_outline().

## Active bindings

- `name`:

  the name of this layer, used for referencing in layer and panel
  modifications

- `all_na`:

  whether all values for this layer are NA in the `data` field

- `definition`:

  a character string specifying the image name or contrast that defines
  this layer

- `source`:

  a character string specifying the layer source within a relevant
  ggbrain_slices object. This is used to lookup the right layer
  information when combining slices and layers together Note that
  multiple layers can potentially have the same source, which is why a
  1:1 mapping to name does not work

- `data`:

  the data.frame containing relevant data for this layer.

- `show_legend`:

  a logical indicating whether to show or hide the fill/color scale

- `unify_scales`:

  a logical indicating whether to unify scale limits and levels when
  this layer is added across many panels

- `bisided`:

  read-only access to whether this layer uses a bisided color scale

- `categorical_fill`:

  read-only access to whether this layer has a categorical fill scale

- `fill_column`:

  read-only access to layer fill column

- `fill_scale`:

  a scale_fill\_\* object containing the ggplot2 fill scale for this
  layer

- `alpha`:

  sets the alpha transparency of this layer.

- `blur_edge`:

  controls the standard deviation (sigma) of a Gaussian blur applied to
  the layer at the edge

- `trim_threads`:

  iteratively trim any pixels that have fewer than this number of
  neighboring pixels

- `fill_holes`:

  controls the size of holes to be filled for display (in pixels)

- `remove_specks`:

  controls the size of specks to be removed (in pixels)

## Methods

### Public methods

- [`ggbrain_layer$new()`](#method-ggbrain_layer-new)

- [`ggbrain_layer$set_limits()`](#method-ggbrain_layer-set_limits)

- [`ggbrain_layer$set_pos_limits()`](#method-ggbrain_layer-set_pos_limits)

- [`ggbrain_layer$set_neg_limits()`](#method-ggbrain_layer-set_neg_limits)

- [`ggbrain_layer$set_breaks()`](#method-ggbrain_layer-set_breaks)

- [`ggbrain_layer$set_pos_breaks()`](#method-ggbrain_layer-set_pos_breaks)

- [`ggbrain_layer$set_neg_breaks()`](#method-ggbrain_layer-set_neg_breaks)

- [`ggbrain_layer$plot()`](#method-ggbrain_layer-plot)

- [`ggbrain_layer$add_to_gg()`](#method-ggbrain_layer-add_to_gg)

- [`ggbrain_layer$get_data()`](#method-ggbrain_layer-get_data)

- [`ggbrain_layer$is_empty()`](#method-ggbrain_layer-is_empty)

- [`ggbrain_layer$clone()`](#method-ggbrain_layer-clone)

------------------------------------------------------------------------

### Method `new()`

create a new ggbrain_layer object

#### Usage

    ggbrain_layer$new(
      name = NULL,
      definition = NULL,
      limits = NULL,
      breaks = integer_breaks(),
      show_legend = TRUE,
      interpolate = NULL,
      unify_scales = TRUE,
      alpha = NULL,
      blur_edge = NULL,
      fill_holes = NULL,
      remove_specks = NULL,
      trim_threads = NULL,
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

  fixed alpha transparency of this layer (use `mapping` for alpha
  mapping\`)

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

- `data`:

  the data.frame containing image data for this layer. Must contain
  "dim1", "dim2", and "value" as columns

------------------------------------------------------------------------

### Method `set_limits()`

set the limits for this layer's scale

#### Usage

    ggbrain_layer$set_limits(limits)

#### Arguments

- `limits`:

  a 2-element numeric vector setting the lower and upper limits on the
  layer's scale

------------------------------------------------------------------------

### Method `set_pos_limits()`

set the limits for this layer's positive scale (only relevant to
bisided)

#### Usage

    ggbrain_layer$set_pos_limits(limits)

#### Arguments

- `limits`:

  a 2-element numeric vector setting the lower and upper limits on the
  layer's positive scale

------------------------------------------------------------------------

### Method `set_neg_limits()`

set the limits for this layer's positive scale (only relevant to
bisided)

#### Usage

    ggbrain_layer$set_neg_limits(limits)

#### Arguments

- `limits`:

  a 2-element numeric vector setting the lower and upper limits on the
  layer's positive scale

------------------------------------------------------------------------

### Method `set_breaks()`

set the breaks element of this layer's scale

#### Usage

    ggbrain_layer$set_breaks(breaks)

#### Arguments

- `breaks`:

  a function used to label the breaks

------------------------------------------------------------------------

### Method `set_pos_breaks()`

set the breaks element of this layer's positive scale (only relevant to
bisided)

#### Usage

    ggbrain_layer$set_pos_breaks(breaks)

#### Arguments

- `breaks`:

  a function used to label the positive breaks

------------------------------------------------------------------------

### Method `set_neg_breaks()`

set the breaks element of this layer's negative scale (only relevant to
bisided)

#### Usage

    ggbrain_layer$set_neg_breaks(breaks)

#### Arguments

- `breaks`:

  a function used to label the negative breaks

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

plot this layer alone (mostly for debugging)

#### Usage

    ggbrain_layer$plot()

------------------------------------------------------------------------

### Method `add_to_gg()`

method to add this layer to an existing ggplot object

#### Usage

    ggbrain_layer$add_to_gg(base_gg)

#### Arguments

- `base_gg`:

  the ggplot object to which we add the layer

------------------------------------------------------------------------

### Method `get_data()`

return the data.frame associated with this layer

#### Usage

    ggbrain_layer$get_data(add_layer_name = FALSE)

#### Arguments

- `add_layer_name`:

  if TRUE, adds a `layer_name` column to the data.frame for
  record-keeping. Default: FALSE.

------------------------------------------------------------------------

### Method `is_empty()`

returns TRUE if all values are NA or if the data has 0 rows

#### Usage

    ggbrain_layer$is_empty()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggbrain_layer$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
