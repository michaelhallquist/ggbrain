# Internal function to compute cluster slice locations

Internal function to compute cluster slice locations

## Usage

``` r
compute_cluster_slices(
  images,
  definition,
  nclusters,
  min_clust_size,
  plane,
  nn,
  outline = FALSE,
  outline_color = NULL,
  outline_size = 1L,
  outline_scale = NULL,
  outline_show_legend = NULL
)
```

## Arguments

- images:

  A ggbrain_images object or named character vector of image paths

- definition:

  The definition/contrast string to apply

- nclusters:

  Maximum number of clusters

- min_clust_size:

  Minimum cluster size in voxels

- plane:

  The plane for slice selection

- nn:

  Neighborhood connectivity (1, 2, or 3)

## Value

A list with elements: coordinates (character vector of slice coords) and
cluster_data (data.frame with cluster details). When outlines are
requested, the list also includes `labeled_volume` (cluster ids
restricted to the top clusters), `outline_palette` (colors for each
cluster), `outline_scale` (ggplot2 scale, if provided), `outline_size`
(integer), and `outline_show_legend` (logical).
