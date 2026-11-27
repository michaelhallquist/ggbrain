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
  outline_show_legend = NULL,
  sided = c("bisided", "two_sided"),
  contrasts = NULL,
  slice_at = "center_of_mass"
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

- outline:

  Logical. If `TRUE`, a cluster outline mask is generated and
  automatically added as a
  [`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md)
  layer during rendering. Default: `FALSE`

- outline_color:

  A single color string for all cluster outlines. If `NULL` (default),
  distinct colors are assigned to each cluster using the "Dark 3" HCL
  palette. Mutually exclusive with `outline_scale`.

- outline_size:

  Integer specifying the outline thickness in pixels. Default: 1

- outline_scale:

  A ggplot2 scale object (e.g., `scale_fill_manual`) to use for cluster
  outline colors. Mutually exclusive with `outline_color`.

- outline_show_legend:

  Logical. Whether to show a legend for cluster outlines. If `NULL`
  (default), a legend is shown when multiple clusters exist.

- sided:

  Sidedness for clustering thresholded images. See
  [`cluster_slices()`](https://michaelhallquist.github.io/ggbrain/reference/cluster_slices.md).

- contrasts:

  Optional named character strings containing numeric/logical contrast
  expressions.

- slice_at:

  Location within each cluster used to select a slice.
  `"center_of_mass"` (default) uses the cluster's center of mass.
  `"peak"` uses the voxel with the largest absolute value in the
  clustering image or contrast. `"nearest"` uses the cluster voxel
  closest to the center of mass by Euclidean distance in world
  coordinates.

## Value

A list with elements: coordinates (character vector of slice coords) and
cluster_data (data.frame with cluster details). When outlines are
requested, the list also includes `labeled_volume` (cluster ids
restricted to the top clusters), `outline_palette` (colors for each
cluster), `outline_scale` (ggplot2 scale, if provided), `outline_size`
(integer), and `outline_show_legend` (logical).
