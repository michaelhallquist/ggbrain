# Annotate crosshairs at user-specified world (xyz) coordinates

Annotate crosshairs at user-specified world (xyz) coordinates

## Usage

``` r
annotate_crosshairs(
  xyz,
  color = "white",
  linewidth = 0.4,
  alpha = 0.7,
  linetype = "dashed",
  tol = 1,
  ...
)
```

## Arguments

- xyz:

  A numeric matrix or data.frame with columns `x`, `y`, and `z` giving
  world-space coordinates (in mm) at which to draw crosshairs.

- color:

  Line color for the crosshairs. Default: "white".

- linewidth:

  Line width passed to
  [`ggplot2::annotate`](https://ggplot2.tidyverse.org/reference/annotate.html).
  Default: 0.4.

- alpha:

  Line alpha. Default: 0.7.

- linetype:

  Line type (e.g., "dashed"). Default: "dashed".

- tol:

  Tolerance (in mm) for matching a coordinate to a displayed slice along
  its slicing axis. Default: 1.

- ...:

  Additional arguments passed to
  [`ggplot2::annotate`](https://ggplot2.tidyverse.org/reference/annotate.html)
  for the crosshair segments.

## Value

A `ggb` object with action "add_annotations"

## Details

Crosshairs are drawn on slices whose slicing axis (e.g., z for axial)
falls within `tol` mm of the requested coordinate. If the coordinate on
the slicing axis is `NA`, the crosshair is shown on all slices along
that axis where the remaining two axes match. For example,
`x=0, y=0, z=NA` will show a crosshair on every axial slice at x/y=0
(within the nearest in-plane voxel). Coordinates are provided in world
space to match the
[`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md)
API.

## Examples

``` r
if (FALSE) { # \dontrun{
# Show crosshairs on any axial slice at x/y = 0
ggbrain() +
  images(c(underlay = underlay_2mm)) +
  slices(c("z = 20", "z = 40")) +
  geom_brain("underlay") +
  annotate_crosshairs(data.frame(x = 0, y = 0, z = NA), linewidth = 0.6)
} # }
```
