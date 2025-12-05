# Clusterize a contrast/image and display clusters as categorical fill

Clusterize a contrast/image and display clusters as categorical fill

## Usage

``` r
geom_brain_clusterized(
  definition,
  name = NULL,
  nclusters = 10,
  min_clust_size = 1,
  nn = 3,
  fill_scale = NULL,
  show_legend = TRUE,
  cluster_info = c("number", "voxels"),
  blur_edge = NULL,
  fill_holes = NULL,
  remove_specks = NULL,
  trim_threads = NULL
)
```

## Arguments

- definition:

  a character string of the contrast or image definition to cluster
  (e.g., `"overlay[overlay > 3]"`)

- name:

  the name of this layer, used for referencing in layer and panel
  modifications

- nclusters:

  maximum number of clusters to retain (largest by size). Default: 10

- min_clust_size:

  minimum cluster size in voxels. Default: 1

- nn:

  Neighborhood connectivity for defining clusters (1 = 6-connectivity, 2
  = 18, 3 = 26). Default: 3

- fill_scale:

  a ggplot2 discrete scale_fill\_\* object to use for mapping cluster
  ids. If NULL, an automatic palette is used.

- show_legend:

  logical; whether to show the cluster legend. Default: TRUE

- cluster_info:

  character vector indicating which fields to include in cluster labels
  for the legend. Supported values: "number" (cluster index), "voxels"
  (voxel count), "size" (mm^3 volume). Order is respected.

- blur_edge, fill_holes, remove_specks, trim_threads:

  optional image refinements passed to
  [`geom_brain()`](https://michaelhallquist.github.io/ggbrain/reference/geom_brain.md)

## Value

a `ggb` object that adds the clusterized layer

## Details

This helper clusters the given definition, adds the labeled cluster
image to the plot, and draws it with a categorical fill. It does not
select slices; combine with
[`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md)
separately.
