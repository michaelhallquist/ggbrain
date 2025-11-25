# Adds the coordinate labels to each panel based on the location of the slice along the slicing axis (e.g., z = 15)

Adds the coordinate labels to each panel based on the location of the
slice along the slicing axis (e.g., z = 15)

## Usage

``` r
annotate_coordinates(x = "right", y = "bottom", ...)
```

## Arguments

- x:

  the x position of the coordinate label. If numeric, it is assumed to
  be the pixel position along the x axis (e.g., 26). In addition,
  convenience values of `"left"`, `"right"`, or `"q[1-100]"` can be used
  to look up the left-most, right-most, or quantile-based positions
  along the x axis.

- y:

  the y position of the coordinate label. If numeric, it is assumed to
  be the pixel position along the y axis (e.g., 26). In addition,
  convenience values of 'top', `"bottom"`, or `"q[1-100]"` can be used
  to look up the top-most, bottom-most, or quantile-based positions
  along the y axis.

- ...:

  any other arguments to ggplot2::annotate, which will be passed through
  to each panel

## Value

a `ggb` object with the action 'add_annotations', used in a `ggbrain`
addition chain
