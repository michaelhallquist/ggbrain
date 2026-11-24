# Adds custom annotations to a single panel on the ggbrain plot

Adds custom annotations to a single panel on the ggbrain plot

## Usage

``` r
annotate_slice(slice_index = NULL, ...)
```

## Arguments

- slice_index:

  the slice number to which this annotation is added. These are numbered
  in the wrapping order from patchwork::wrap_plots, which will normally
  go from top-left to bottom-right.

- ...:

  Additional parameters passed to ggplot2::annotate such as `label` or
  `geom`

## Value

a `ggb` object with the relevant annotations field and an action of
"add_annotations"

## Details

For annotation coordinates such as `x`, `y`, or `xmin`, you may pass in
a number. In this case, the value specifies the pixel position along the
relevant axis (e.g., `x=26`). In addition, convenience values of 'left',
'right', and 'middle' can be used for the x axis, and 'top', 'bottom',
and 'middle' for the y axis. 'middle' uses the midpoint of the
coordinate range.

Use `"q1"` through `"q100"` for percentile positions along the relevant
axis. For example, `x = "q25"` uses the 25th percentile. Decimal
fractions also work, so `"q0.25"` is equivalent to `"q25"` and `"q1.0"`
is equivalent to `"q100"`.

N.B. This function only adds a single annotation on a single panel!
