# scale for plotting separate color gradients for positive and negative values

scale for plotting separate color gradients for positive and negative
values

## Usage

``` r
scale_fill_bisided(
  name = ggplot2::waiver(),
  neg_scale = scale_fill_distiller(palette = "Blues", direction = 1),
  pos_scale = scale_fill_distiller(palette = "Reds"),
  symmetric = TRUE
)
```

## Arguments

- name:

  the scale name to be printed in the legend (above positive scale)

- neg_scale:

  a scale_fill\_\* object used for negative values

- pos_scale:

  a scale_fill\_\* object used for positive values

- symmetric:

  if TRUE, the limits of the positive scale will equal the inverse
  limits of the negative scale. Said differently, this makes the
  positive and negative scales symmetric

## Value

a `ggplot2` scale of type `ScaleContinuous` that includes negative and
positive fill scales internally in the `$neg_scale` and `$pos_scale`
elements

## Details

Note that this will absolutely not work as a general purpose ggplot2
scale! The positive/negative combination is achieved by adding two
layers/geoms behind the scenes with different color scale.
