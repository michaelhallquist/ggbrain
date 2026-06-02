# Find 3D connected components separately by positive and negative sign

Find 3D connected components separately by positive and negative sign

## Usage

``` r
find_bisided_3d_clusters(
  mask,
  values,
  min_size = 1,
  nn = 3,
  return_labels = FALSE,
  zero_tol = 1e-06
)
```

## Arguments

- mask:

  A 3D logical array (TRUE for voxels of interest)

- values:

  A 3D numeric array used to separate positive and negative tails

- min_size:

  Minimum cluster size to retain

- nn:

  Neighborhood connectivity (1=6-conn, 2=18-conn, 3=26-conn)

- return_labels:

  If TRUE, attaches the labeled volume (after size filtering) as an
  attribute named `labeled_volume`.

- zero_tol:

  Tolerance for treating voxel values as zero

## Value

A data.frame with columns: cluster_id, side, size, com_i, com_j, com_k
