# Adds a raster layer to the ggbrain plot, displaying pixels from the specified layer definition

Adds a raster layer to the ggbrain plot, displaying pixels from the
specified layer definition

## Usage

``` r
geom_brain(
  definition = NULL,
  name = NULL,
  fill = NULL,
  fill_scale = NULL,
  mapping = NULL,
  limits = NULL,
  breaks = NULL,
  show_legend = TRUE,
  interpolate = FALSE,
  unify_scales = TRUE,
  alpha = NULL,
  blur_edge = NULL,
  fill_holes = NULL,
  remove_specks = NULL,
  trim_threads = NULL
)
```

## Arguments

- definition:

  a character string of the contrast or image definition used to define
  this layer. Can be a simple image name (e.g., 'underlay') or a
  contrast string (e.g., `'overlay[overlay > 5]'`)

- name:

  the name of this layer, used for referencing in layer and panel
  modifications

- fill:

  A character string indicating the color used to fill all non-NA pixels
  in this layer. This is used to set the fill color, in distinction to
  color mapping: `mapping=aes(fill=<variable>)`.

- fill_scale:

  a ggplot scale_fill\_\* object used for mapping the fill column to the
  color of pixels in this layer.

- mapping:

  the aesthetic mapping of the layer data to the display. Should be an
  aes() object and supports `fill` (color of filled pixels). Default is
  `aes(fill=value)`, which maps the numeric value of the layer data to
  the fill color of the squares at each spatial position. For labeled
  data, you might use `aes(fill=<label_col_name>)`.

- limits:

  if provided, sets the upper and lower bounds on the scale

- breaks:

  if provided, a function to draw the breaks on the fill scale

- show_legend:

  if TRUE, show the fill scale in the plot legend. Default is TRUE,
  except when \`name="underlay"“

- interpolate:

  passes to geom_raster and controls whether the fill is interpolated
  over continuous space

- unify_scales:

  if TRUE, when this layer is reused across panels, unify the scales to
  match

- alpha:

  a number between 0 and 1 that sets the alpha transparency of this
  layer. Default: 1

- blur_edge:

  the standard deviation (sigma) of a Gaussian kernel applied to the
  edge of this layer to smooth it. This makes the layer less jagged in
  appearance and is akin to antialiasing.

- fill_holes:

  An optional positive integer specifying the size of holes (in pixels)
  inside clusters to be filled by nearest neighbor imputation. Default:
  0.

- remove_specks:

  An optional positive integer specifying the size of specks (in pixels)
  to be removed from each slice prior to display. Specks are small
  clusters that may be distracting and contribute to a 'salt and pepper'
  appearance.

- trim_threads:

  the minimum number of neighboring pixels (including diagonals) that
  must be present to keep a pixel.

## Value

a ggb object populated with the relevant geom_brain and the action of
'add_layers'

## Details

Note that the fill_scale and limits must be specified at the time of the
geom_brain creation in order for them to be mapped properly within
ggplot. Because we overlay many raster layers in a ggplot object that
all use the fill aesthetic mapping, it becomes hard to map the color
scales after the layer is created using the typical + scale_fill\_\*
syntax, and similarly for scale limits.

## Examples

``` r
  # T1-weighted template
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # signed reward prediction error map
  signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = t1, overlay = signed_pe)) +
    slices(c("x = 25%", "x = 75%")) +
    geom_brain("underlay") +
    geom_brain(definition="overlay[overlay > 1]", fill_scale=ggplot2::scale_fill_viridis_c("pos z"))
```
