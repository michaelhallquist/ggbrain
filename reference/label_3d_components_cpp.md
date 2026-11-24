# Label connected components in a 3D logical array

Label connected components in a 3D logical array

## Usage

``` r
label_3d_components_cpp(mask, nn = 3L, min_size = 1L, return_labels = FALSE)
```

## Arguments

- mask:

  A 3D logical array with TRUE for foreground voxels.

- nn:

  Connectivity level: 1 for faces, 2 for faces and edges, or 3 for
  faces, edges, and corners.

- min_size:

  Minimum component size to include in the returned statistics.

- return_labels:

  Whether to return the labeled volume.

## Value

A list containing component statistics and, when requested, labels.
