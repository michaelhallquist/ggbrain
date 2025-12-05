# Find 3D connected components in a binary volume

Find 3D connected components in a binary volume

## Usage

``` r
find_3d_clusters(mask, min_size = 1, nn = 3, return_labels = FALSE)
```

## Arguments

- mask:

  A 3D logical array (TRUE for voxels of interest)

- min_size:

  Minimum cluster size to retain

- nn:

  Neighborhood connectivity (1=6-conn, 2=18-conn, 3=26-conn)

- return_labels:

  If TRUE, attaches the labeled volume (after size filtering) as an
  attribute named `labeled_volume`.

## Value

A data.frame with columns: cluster_id, size, com_i, com_j, com_k
