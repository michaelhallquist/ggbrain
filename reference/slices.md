# Adds slices to the ggbrain plot, including additional panel aesthetics

Adds slices to the ggbrain plot, including additional panel aesthetics

## Usage

``` r
slices(
  coordinates = NULL,
  title = NULL,
  bg_color = NULL,
  text_color = NULL,
  border_color = NULL,
  border_size = NULL,
  xlab = NULL,
  ylab = NULL,
  theme_custom = NULL
)
```

## Arguments

- coordinates:

  a character vector specifying the x, y, or z coordinates of the slices
  to be added. Can also be a `cluster_slices_spec` object from
  [`cluster_slices()`](https://michaelhallquist.github.io/ggbrain/reference/cluster_slices.md),
  which will compute slice locations based on cluster centers of mass.

- title:

  a title for the slice panels added to the ggplot object using
  `ggtitle()`

- bg_color:

  the color used for the background of the panels. Default: `'gray10'`
  (nearly black)

- text_color:

  the color used for text displayed on the panels. Default: `'white'`.

- border_color:

  the color used for drawing a border around on the panels. Default:
  `'gray50'` (though borders are not drawn by default).

- border_size:

  the size of the border line drawn around the panels. Default: NULL. If
  this value is greater than zero, a border of this size and with color
  `border_color` will be drawn around the panels.

- xlab:

  The label to place on x axis. Default is NULL.

- ylab:

  The label to place on y axis. Default is NULL.

- theme_custom:

  Any custom theme() settings to be added to the panels.

## Value

a `ggb` object with the relevant slices and an action of 'add_slices'

## Details

note that if you pass in multiple coordinates (as a vector), the
`title`, `bg_color`, and other attributes will be reused for all slices
added by this operation. Thus, if you want to customize specific slices
or groups of slices, use multiple addition operations, as in
`slices(c('x=10', 'y=15'), bg_color='white') + slices(c('x=18', 'y=22'), bg_color='black')`.

You can also pass the result of
[`cluster_slices()`](https://michaelhallquist.github.io/ggbrain/reference/cluster_slices.md)
to automatically select slices based on cluster centers of mass in a
thresholded image.

## Examples

``` r
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  gg_obj <- ggbrain() +
    images(c(underlay = t1)) +
    slices(c("x = 25%", "x = 75%"), border_color = "blue")
```
