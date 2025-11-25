# R6 class for a single panel of a ggbrain image

R6 class for a single panel of a ggbrain image

R6 class for a single panel of a ggbrain image

## Value

a `ggbrain_panel` R6 class with fields related to a panel on the
`ggbrain` plot

## Details

Note that this class is exported only for power users and rarely needs
to be called directly in typical use of the package. Instead, look at
[`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md).

## Public fields

- `gg`:

  The ggplot object that contains the panel

## Methods

### Public methods

- [`ggbrain_panel$new()`](#method-ggbrain_panel-new)

- [`ggbrain_panel$reset_limits()`](#method-ggbrain_panel-reset_limits)

- [`ggbrain_panel$plot()`](#method-ggbrain_panel-plot)

- [`ggbrain_panel$add_to_gg()`](#method-ggbrain_panel-add_to_gg)

- [`ggbrain_panel$add_layer()`](#method-ggbrain_panel-add_layer)

- [`ggbrain_panel$remove_layers()`](#method-ggbrain_panel-remove_layers)

- [`ggbrain_panel$get_data()`](#method-ggbrain_panel-get_data)

- [`ggbrain_panel$get_layer_names()`](#method-ggbrain_panel-get_layer_names)

- [`ggbrain_panel$get_layers()`](#method-ggbrain_panel-get_layers)

- [`ggbrain_panel$set_layer_order()`](#method-ggbrain_panel-set_layer_order)

- [`ggbrain_panel$clone()`](#method-ggbrain_panel-clone)

------------------------------------------------------------------------

### Method `new()`

create a new ggbrain_panel object

#### Usage

    ggbrain_panel$new(
      layers = NULL,
      title = NULL,
      bg_color = NULL,
      text_color = NULL,
      border_color = NULL,
      border_size = NULL,
      xlab = NULL,
      ylab = NULL,
      theme_custom = NULL,
      annotations = NULL,
      region_labels = NULL
    )

#### Arguments

- `layers`:

  a list of ggbrain_layer objects to form the panel

- `title`:

  a title for the panel added to the ggplot object using ggtitle()

- `bg_color`:

  the color used for the background of the plot. Default: 'gray10'
  (nearly black)

- `text_color`:

  the color used for text displayed on the plot. Default: 'white'.

- `border_color`:

  the color used for drawing a border around on the plot. Default:
  'gray50' (though borders are not drawn by default).

- `border_size`:

  the size of the border line drawn around the panel. Default: NULL. If
  this value is greater than zero, a border of this size and with color
  `border_color` will be drawn around the panel

- `xlab`:

  The label to place on x axis. Default is NULL.

- `ylab`:

  The label to place on y axis. Default is NULL.

- `theme_custom`:

  Any custom theme() settings to be added to the plot

- `annotations`:

  a data.frame containing all annotations to be added to this plot. Each
  row is cleaned up and passed to ggplot2::annotate()

- `region_labels`:

  a list of ggbrain_label objects with data for plotting region labels
  on this panel

------------------------------------------------------------------------

### Method `reset_limits()`

Reset the scale limits for the specified layers

#### Usage

    ggbrain_panel$reset_limits(layer_names)

#### Arguments

- `layer_names`:

  not implemented yet

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

plot the panel

#### Usage

    ggbrain_panel$plot(use_global_limits = TRUE)

#### Arguments

- `use_global_limits`:

  Not implemented at present

------------------------------------------------------------------------

### Method `add_to_gg()`

add one or more custom ggplot settings to the panel

#### Usage

    ggbrain_panel$add_to_gg(list_args)

#### Arguments

- `list_args`:

  A list containing elements to add to the ggplot object

#### Details

Note that passing in an expression such as theme_bw() + ggtitle("hello")
will not work because it creates an object that cannot be added
sequentially to the ggplot. As noted in ggplot2's documentation
(https://ggplot2.tidyverse.org/reference/gg-add.html), to
programmatically add elements to a ggplot, pass in a list where each
element is added sequentially

------------------------------------------------------------------------

### Method `add_layer()`

adds a ggplot_layer object to the panel

#### Usage

    ggbrain_panel$add_layer(layer_obj)

#### Arguments

- `layer_obj`:

  a ggbrain_layer object to add to the panel

------------------------------------------------------------------------

### Method `remove_layers()`

removes one or more layers by name

#### Usage

    ggbrain_panel$remove_layers(layer_names)

#### Arguments

- `layer_names`:

  a character string of the layers to remove from the panel

------------------------------------------------------------------------

### Method `get_data()`

returns the data for all layers in the object

#### Usage

    ggbrain_panel$get_data()

------------------------------------------------------------------------

### Method `get_layer_names()`

returns the names of the layers in this panel, ordered from bottom to
top

#### Usage

    ggbrain_panel$get_layer_names()

------------------------------------------------------------------------

### Method `get_layers()`

returns a list of ggbrain_layer objects that comprise this panel

#### Usage

    ggbrain_panel$get_layers()

------------------------------------------------------------------------

### Method `set_layer_order()`

sets the order of layers from bottom to top based on the layer names
provided

#### Usage

    ggbrain_panel$set_layer_order(ordered_names = NULL)

#### Arguments

- `ordered_names`:

  the names of the layers in the desired order from bottom to top. All
  layer names must be provided, not just a subset

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggbrain_panel$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
