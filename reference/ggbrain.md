# create ggb container object for a given plot

create ggb container object for a given plot

## Usage

``` r
ggbrain(
  images = NULL,
  slices = NULL,
  title = NULL,
  bg_color = "grey8",
  text_color = "grey92",
  base_size = 14
)
```

## Arguments

- images:

  a character vector or existing ggbrain_images object defining which
  images should be included in this plot

- slices:

  a set of slices to be added to the plot

- title:

  the overall title to be added to the plot

- bg_color:

  The background color of the overall plot

- text_color:

  The default text color of the overall plot (passes through to panels)

- base_size:

  The base size of fonts used in the plot (cf. `theme_minimal`)

## Value

a `ggb` object containing basic information for a `ggbrain` plot such as
background color, text color, and font size
