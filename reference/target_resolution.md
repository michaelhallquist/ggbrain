# Set the target resolution for resampling slices in a ggbrain plot

Set the target resolution for resampling slices in a ggbrain plot

## Usage

``` r
target_resolution(voxel_size, interpolation = "cubic")
```

## Arguments

- voxel_size:

  A positive number specifying the target voxel size in millimeters
  (e.g., 1 for 1mm isotropic). The slices will be resampled to this
  resolution before display. If the input images have larger voxels
  (e.g., 2.5mm), they will be upsampled to the target resolution. If the
  input images have smaller voxels (e.g., 0.5mm), they will be
  downsampled.

- interpolation:

  A character string specifying the interpolation method to use. Options
  are:

  - `"nearest"` (value 1): Nearest neighbor interpolation. Fast but can
    produce blocky results.

  - `"linear"` (value 2): Linear interpolation. Smooth but may blur
    edges.

  - `"cubic"` (value 5): Cubic interpolation (default). Good balance of
    smoothness and edge preservation.

  - `"lanczos"` (value 6): Lanczos interpolation. High quality but
    slower.

## Value

A `ggb` object with the target resolution settings and an action of
'set_target_resolution'

## Details

This function allows you to display brain images at a different
resolution than their native voxel size. For example, if your functional
MRI data have 2.5mm voxels but you want to display them at 1mm
resolution for publication-quality figures, you can use
`target_resolution(1)` to upsample.

Conversely, if you have high-resolution images (e.g., 0.5mm) that are
slow to render or result in very large file sizes, you can downsample
them using `target_resolution(2)` to display at 2mm.

The resampling is performed on each 2D slice after extraction, which is
more efficient than resampling the entire 3D volume. The interpolation
is applied to all images in the plot. For labeled/categorical images,
nearest neighbor interpolation is always used to preserve integer
values.

Note that upsampling does not add information to the image – it simply
provides a smoother visual representation. For the highest quality
results with functional overlays on anatomical underlays, consider using
a high-resolution anatomical template (e.g., 1mm) as your underlay.

## Examples

``` r
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # Display 2mm images at 1mm resolution (upsampling) using cubic interpolation
  gg_obj <- ggbrain() +
    images(c(underlay = t1, overlay = overlay)) +
    slices(c("x = 0", "z = 20")) +
    target_resolution(1, interpolation = "cubic") +
    geom_brain("underlay") +
    geom_brain("overlay")

  # Downsample to 4mm for faster rendering or smaller file sizes
  gg_obj_small <- ggbrain() +
    images(c(underlay = t1)) +
    slices(c("x = 0", "z = 20")) +
    target_resolution(4, interpolation = "linear") +
    geom_brain("underlay")
```
