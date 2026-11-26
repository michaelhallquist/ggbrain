# Build a manual ggplot2 fill scale from a color lookup table (LUT)

Constructs a
[`scale_fill_manual`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
mapping region labels or integer codes to the hexadecimal colors defined
in a lookup table read with
[`read_lut()`](https://michaelhallquist.github.io/ggbrain/reference/read_lut.md).
Works for both `fill_scale` in
[`geom_brain()`](https://michaelhallquist.github.io/ggbrain/reference/geom_brain.md)
and `outline_scale` in
[`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md).

## Usage

``` r
scale_fill_lut(
  lut,
  name = ggplot2::waiver(),
  by = "label",
  na.value = "transparent",
  ...
)
```

## Arguments

- lut:

  Either a `data.frame` (typically returned by
  [`read_lut()`](https://michaelhallquist.github.io/ggbrain/reference/read_lut.md))
  containing a `color` column and the column named in `by`, or a path to
  a LUT file to be parsed with
  [`read_lut()`](https://michaelhallquist.github.io/ggbrain/reference/read_lut.md).

- name:

  Optional scale title passed to
  [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).
  Defaults to
  [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html),
  which falls back to the layer name in `ggbrain`.

- by:

  Character string naming the column in `lut` used as keys for the color
  mapping. Defaults to `"label"` (for `aes(fill = label)`); use
  `"value"` when mapping integer atlas values directly (e.g.,
  `aes(fill = factor(value))`), or any custom column name present in
  `lut`.

- na.value:

  Color used for missing values. Defaults to `"transparent"`.

- ...:

  Additional arguments passed to
  [`scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  (e.g., `breaks`, `labels`, `limits`, `drop`, `guide`).

## Value

A discrete `ggplot2` fill scale (`ScaleDiscrete`) mapping `by` keys to
LUT hex colors.

## See also

[`read_lut()`](https://michaelhallquist.github.io/ggbrain/reference/read_lut.md),
[`geom_brain()`](https://michaelhallquist.github.io/ggbrain/reference/geom_brain.md),
[`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md)

## Examples

``` r
lut_file <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")
yeo_lut <- read_lut(lut_file)
yeo_lut$network <- c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic", "Cont", "Default")

# Ready to pass to geom_brain(..., fill_scale = scale_fill_lut(yeo_lut, by = "network"))
sc <- scale_fill_lut(yeo_lut, name = "Yeo 7 Networks", by = "network")
sc
#> <ggproto object: Class ScaleDiscrete, Scale, gg>
#>     aesthetics: fill
#>     axis_order: function
#>     break_info: function
#>     break_positions: function
#>     breaks: waiver
#>     call: call
#>     clone: function
#>     dimension: function
#>     drop: TRUE
#>     expand: waiver
#>     fallback_palette: function
#>     get_breaks: function
#>     get_breaks_minor: function
#>     get_labels: function
#>     get_limits: function
#>     get_transformation: function
#>     guide: legend
#>     is_discrete: function
#>     is_empty: function
#>     labels: waiver
#>     limits: function
#>     make_sec_title: function
#>     make_title: function
#>     map: function
#>     map_df: function
#>     minor_breaks: waiver
#>     n.breaks.cache: NULL
#>     na.translate: TRUE
#>     na.value: transparent
#>     name: Yeo 7 Networks
#>     palette: function
#>     palette.cache: NULL
#>     position: left
#>     range: environment
#>     rescale: function
#>     reset: function
#>     train: function
#>     train_df: function
#>     transform: function
#>     transform_df: function
#>     super:  <ggproto object: Class ScaleDiscrete, Scale, gg>
```
