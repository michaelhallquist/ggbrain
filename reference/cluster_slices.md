# Find slice locations based on cluster centers of mass in a 3D brain image

This function identifies connected clusters in a brain image (after
applying an optional definition/contrast) and returns slice coordinates
corresponding to the center of mass of each cluster. This is useful for
automatically selecting slices that display the most relevant activation
clusters.

## Usage

``` r
cluster_slices(
  images = NULL,
  layer = NULL,
  definition = NULL,
  nclusters = 10,
  min_clust_size = 1,
  plane = "axial",
  nn = 3,
  outline = FALSE,
  outline_color = NULL,
  outline_size = 1L,
  outline_scale = NULL,
  outline_show_legend = NULL
)
```

## Arguments

- images:

  A ggbrain_images object or a named character vector of image file
  paths. Required for computing cluster locations.

- layer:

  The name of a geom_brain layer whose definition should be used for
  clustering. Mutually exclusive with `definition`. When using `layer`,
  the function returns a deferred specification that is resolved during
  rendering when layer definitions are available.

- definition:

  A definition string (same syntax as geom_brain) specifying the
  image/contrast to use for clustering. Examples: `"overlay"`,
  `"overlay[overlay > 2.5]"`, `"overlay[abs(overlay) > 2.5]"`. Mutually
  exclusive with `layer`.

- nclusters:

  Maximum number of clusters to return slices for. Clusters are sorted
  by size (descending), and the top `nclusters` are selected. Default:
  10

- min_clust_size:

  Minimum cluster size in voxels. Clusters smaller than this are
  ignored. Default: 1

- plane:

  The plane for slice selection: `"axial"` (z), `"sagittal"` (x), or
  `"coronal"` (y). Default: `"axial"`

- nn:

  Neighborhood connectivity for defining clusters. Must be 1, 2, or 3:

  - `nn=1`: 6-connectivity (faces touching only)

  - `nn=2`: 18-connectivity (faces and edges touching)

  - `nn=3`: 26-connectivity (faces, edges, and corners touching)

  Default: 3 (most inclusive)

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

## Value

If `images` is provided along with `definition`, returns a character
vector of slice coordinates (e.g., `c("z = 10.5", "z = -3.2")`) suitable
for passing to
[`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md).
If `layer` is specified or `images` is not provided, returns a deferred
specification object (class `"cluster_slices_spec"`) that will be
resolved during the ggbrain rendering pipeline.

## Details

The function performs 3D connected component labeling on the
thresholded/masked image to identify distinct clusters. For each cluster
meeting the minimum size criterion, the center of mass is computed.
Slices are then selected at the center of mass coordinates along the
specified plane.

When `outline = TRUE`, a thin outline mask is also generated for the
selected clusters. This outline can be automatically added as a
[`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md)
layer during rendering so that clusters are highlighted on each slice.
By default, distinct colors are assigned to each cluster using a
categorical palette, but a single `outline_color` or a custom vector of
colors can also be provided. The outline thickness can be controlled via
`outline_size`.

The neighborhood connectivity parameter (`nn`) controls how voxels are
considered connected:

- `nn=1` (6-connectivity): Only voxels sharing a face are connected

- `nn=2` (18-connectivity): Voxels sharing a face or edge are connected

- `nn=3` (26-connectivity): Voxels sharing a face, edge, or corner are
  connected

## Examples

``` r
if (FALSE) { # \dontrun{
  # Using a definition string directly
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = t1, overlay = overlay)) +
    slices(cluster_slices(
      images = c(overlay = overlay),
      definition = "overlay[abs(overlay) > 2.5]",
      nclusters = 5,
      min_clust_size = 50,
      plane = "axial"
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay[abs(overlay) > 2.5]")

  # Or defer resolution until render time (images from ggbrain pipeline)
  gg_obj <- ggbrain() +
    images(c(underlay = t1, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 2.5]",
      nclusters = 5
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay[abs(overlay) > 2.5]")
} # }
```
