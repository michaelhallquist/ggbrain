# R6 class for adding labels to a ggbrain_panel

R6 class for adding labels to a ggbrain_panel

R6 class for adding labels to a ggbrain_panel

## Value

a `ggbrain_label` R6 class containing fields related to ggbrain plot
labels

## Public fields

- `addl_args`:

  a named list of additional argument to be passed to
  geom_text/geom_label at render

## Active bindings

- `data`:

  a data.frame containing labels to be printed on the panel. Must
  contain dim1, dim2, and label as columns. The dim1 and dim2 columns
  control where the labels will appear on the panel

- `image`:

  A character string specifying the image to which these labels pertain

- `label_column`:

  A character string indicating which data.frame column should be used
  for drawing labels

- `min_px`:

  A positive integer indicating the minimum number of pixels present on
  slice that will generate a label

## Methods

### Public methods

- [`ggbrain_label$new()`](#method-ggbrain_label-new)

- [`ggbrain_label$add_to_gg()`](#method-ggbrain_label-add_to_gg)

- [`ggbrain_label$clone()`](#method-ggbrain_label-clone)

------------------------------------------------------------------------

### Method `new()`

create a new ggbrain_label object

#### Usage

    ggbrain_label$new(
      data = NULL,
      geom = "text",
      image = NULL,
      label_column = NULL,
      min_px = NULL,
      ...
    )

#### Arguments

- `data`:

  a data.frame containing labels to be printed on the panel. Must
  contain dim1, dim2, and label as columns. The dim1 and dim2 columns
  control where the labels will appear on the panel

- `geom`:

  The geom type to be plotted. Must be "text" or "label", corresponding
  to geom_text and geom_label, respectively.

- `image`:

  A string specifying the image to which these labels pertain

- `label_column`:

  the column in `data` that should be drawn as labels on the plot

- `min_px`:

  the minimum number of pixels on a slice required to display label.
  Default: 1

- `...`:

  All other arguments that will be passed directly to geom_text or
  geom_label such as hjust, size, and color

------------------------------------------------------------------------

### Method `add_to_gg()`

add this text layer to an existing ggplot object

#### Usage

    ggbrain_label$add_to_gg(base_gg)

#### Arguments

- `base_gg`:

  the ggplot object to which we add the layer

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggbrain_label$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
