# Select slices through clusters in a 3D brain image

This function identifies connected clusters in a brain image (after
applying an optional definition/contrast) and returns slice coordinates
for the largest clusters. Slices can pass through the center of mass,
the peak, or the voxel nearest the center of mass within each cluster.

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
  outline_show_legend = NULL,
  sided = c("bisided", "two_sided"),
  slice_at = c("center_of_mass", "peak", "nearest")
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
  `"overlay[overlay > 2.5]"`, `"overlay[abs(overlay) > 2.5]"`, or
  `"a - b"`. A named contrast created upstream with
  [`define()`](https://michaelhallquist.github.io/ggbrain/reference/define.md)
  can also be referenced when `cluster_slices()` is used within a
  [`ggbrain()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain.md)
  pipeline. Mutually exclusive with `layer`.

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

- sided:

  Sidedness for clustering thresholded images. `"bisided"` (default)
  clusters positive and negative surviving voxels separately, matching
  AFNI's `3dClusterize -bisided` behavior. `"two_sided"` allows all
  surviving voxels to connect regardless of sign.

- slice_at:

  Location within each cluster used to select a slice.
  `"center_of_mass"` (default) uses the cluster's center of mass.
  `"peak"` uses the voxel with the largest absolute value in the
  clustering image or contrast. `"nearest"` uses the cluster voxel
  closest to the center of mass by Euclidean distance in world
  coordinates.

## Value

If `images` is provided along with `definition`, returns a list. Pass
its `$coordinates` character vector to
[`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md)
and use `$cluster_data` to inspect cluster sizes and centers. For
`"peak"` and `"nearest"`, the table also reports the selected voxel
indices (`slice_i`, `slice_j`, `slice_k`) and its signed `slice_value`.
When outlines are requested, `$labeled_volume` contains the selected
cluster IDs. If `layer` is specified or `images` is omitted, returns a
deferred specification object (class `"cluster_slices_spec"`) that is
resolved during rendering.

## Details

The function performs 3D connected component labeling on the
thresholded/masked image to identify distinct clusters. Clusters are
ranked by size for all slice locations. The `"peak"` and `"nearest"`
options always select a voxel inside the cluster and return its slice in
voxel coordinates. For a negative cluster, `"peak"` selects the most
negative voxel. Ties use the first voxel in array order (`i` varies
fastest). For a logical definition, all included voxels have value 1.
Use a masked numeric definition such as `"overlay[abs(overlay) > 3]"` to
select peaks by statistic magnitude.

Contrast definitions can be supplied in two ways:

1.  Put the complete image, arithmetic, logical, or masking expression
    directly in `definition`, such as `"(a - b)[abs(a - b) > 2]"`. This
    inline pathway works for both immediate calls with `images` and
    deferred calls inside a
    [`ggbrain()`](https://michaelhallquist.github.io/ggbrain/reference/ggbrain.md)
    pipeline.

2.  Add a named contrast upstream with
    [`define()`](https://michaelhallquist.github.io/ggbrain/reference/define.md),
    then reference that name in `definition`. For example, use
    `define(difference := a - b)` followed by
    `cluster_slices(definition = "difference")`. This pathway is
    resolved during rendering and keeps the same named contrast
    available to other layers such as `geom_brain("difference")`.

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
  # Pathway 1: put the complete expression directly in cluster_slices()
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  inline_plot <- ggbrain() +
    images(c(underlay = t1, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 2.5]",
      nclusters = 5,
      min_clust_size = 50,
      plane = "axial"
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay[abs(overlay) > 2.5]")

  # Pathway 2: define a reusable contrast upstream, then reference its name
  defined_plot <- ggbrain() +
    images(c(underlay = t1, overlay = overlay)) +
    define(active := overlay[abs(overlay) > 2.5]) +
    slices(cluster_slices(
      definition = "active",
      nclusters = 5
    )) +
    geom_brain("underlay") +
    geom_brain("active")
} # }
```
