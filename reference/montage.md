# Convenience function to add many slices in a montage along one of the 3D planes

Convenience function to add many slices in a montage along one of the 3D
planes

## Usage

``` r
montage(
  plane = NULL,
  n = 12,
  min = 0.1,
  max = 0.9,
  min_coord = NULL,
  max_coord = NULL
)
```

## Arguments

- plane:

  a character string specifying the 3D plane: "sagittal", "axial",
  "coronal", "x", "y", or "z"

- n:

  number of slices to add in this plane. Default: 12

- min:

  the lowest quantile to be included in the montage (between 0 and 1).
  Default: 0.1

- max:

  the highest quantile to be included in the montage (between 0 and 1).
  Default: 0.9

- min_coord:

  the lowest spatial position (in image coordinate space) to be included
  in the montage.

- max_coord:

  the highest spatial position (in image coordinate space) to be
  included in the montage.

## Value

a character string containing the slice positions along the requested
axis

## Details

This can be used with `slices` to make a quick montage, such as
`slices(montage("axial", 10)`.

Also note that use of standardized coordinates (in quantiles, using
`min` and `max`) is mutually exclusive with the the image coordinate
specifications `min_coord` and `max_coord.`

## Examples

``` r
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  gg_obj <- ggbrain() +
    images(c(underlay = t1)) +
    slices(montage("sagittal", 15))
```
