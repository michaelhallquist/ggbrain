# Adds contrast definitions to the ggbrain plot

Adds contrast definitions to the ggbrain plot

## Usage

``` r
define(contrasts = NULL)
```

## Arguments

- contrasts:

  a character vector or list containing contrasts to be computed as part
  of the ggbrain object definition.

## Value

a `ggb` object with the relevant contrasts and an action of
'add_contrasts'

## Details

`contrasts` must take the form of `<name> := <value expression>` or must
use a named vector. Note that defining a contrast does not directly
impact the appearance of the plot unless the contrast is named in a
geom\_\* layer.

Also note that contrasts can be specified in the definition of a layer.
Thus, the `define` function has two primary virtues. First, it allows
for the conceptual separation of contrast definition versus usage inside
a geom\_\* layer, which is particularly useful if a contrast is used
across several layers. Second, it allows downstream layers to further
modify the contrast, such as when we compute a

## Examples

``` r
  # T1-weighted template
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # signed reward prediction error map
  signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # unsigned (absolute value) prediction error map
  abspe <- system.file("extdata", "abspe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # simple example of a difference contrast, separating definition from usage in geom_brain
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    define("signed_gt_abs := signed_pe - abspe") +
    geom_brain("signed_gt_abs")

  # you can also use a named vector in define(), which is equivalent
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    define(c(signed_gt_abs = "signed_pe - abspe")) +
    geom_brain("signed_gt_abs")

  # contrast definitions can also occur inline, yielding equivalent plots
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    geom_brain("signed_pe - abspe")

  # The use of contrasts() is helpful when layers modify the contrast (e.g., subsetting)
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    define(c(signed_gt_abs = "signed_pe - abspe")) +
    geom_brain(
      "signed_gt_abs[signed_gt_abs > 0]",
      fill_scale=ggplot2::scale_fill_distiller("Pos diff", palette = "Reds")
    )
```
