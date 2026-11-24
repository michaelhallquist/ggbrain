# Adds the coordinate labels to each panel based on the location of the slice along the slicing axis (e.g., z = 15)

Adds the coordinate labels to each panel based on the location of the
slice along the slicing axis (e.g., z = 15)

## Usage

``` r
annotate_coordinates(x = "right", y = "bottom", ..., placement = c("panel", "caption"))
```

## Arguments

- x:

  For panel placement, the x position of the coordinate label. If
  numeric, it is assumed to be the pixel position along the x axis
  (e.g., 26). Use `"left"`, `"right"`, or `"middle"` for the left edge,
  right edge, or midpoint of the coordinate range, or `"q[1-100]"` for a
  quantile-based position along the x axis.

- y:

  For panel placement, the y position of the coordinate label. If
  numeric, it is assumed to be the pixel position along the y axis
  (e.g., 26). Use `"top"`, `"bottom"`, or `"middle"` for the top edge,
  bottom edge, or midpoint of the coordinate range, or `"q[1-100]"` for
  a quantile-based position along the y axis.

- ...:

  For panel placement, additional arguments passed to
  [`ggplot2::annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html).
  For captions, styling options are `color` (or `colour`), `size` (in
  mm, as for panel text), `family`, `fontface`, `hjust`, and
  `lineheight`. `slice_index` can restrict either placement to a slice.

- placement:

  `"panel"` (default) draws the label in the image panel. `"caption"`
  puts a right-aligned caption below each image panel and ignores `x`
  and `y`.

## Value

a `ggb` object with the action 'add_annotations', used in a `ggbrain`
addition chain

## Details

With panel placement, `y = "top"` and `y = "bottom"` reserve a band of
background for text or label annotations. `x = "left"` and `x = "right"`
inset the label and supply inward horizontal justification by default.
Use a caption for narrow panels or large text, where the panel band may
be too small.

For quantile positions, `"q1"` means 1% and `"q1.0"` means 100%
(equivalent to `"q100"`).
