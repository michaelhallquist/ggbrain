# Define derived image contrasts

Define derived image contrasts

## Usage

``` r
define(contrasts = NULL)
```

## Arguments

- contrasts:

  One or more named contrast definitions. The preferred form is a single
  unquoted expression, `<name> := <expression>`. Character strings using
  that syntax and named character vectors are retained for compatibility
  and for defining multiple contrasts in one call.

## Value

a `ggb` object with the relevant contrasts and an action of
'add_contrasts'

## Details

`define()` names a derived image that can subsequently be used by
[`geom_brain()`](https://michaelhallquist.github.io/ggbrain/reference/geom_brain.md)
or
[`geom_outline()`](https://michaelhallquist.github.io/ggbrain/reference/geom_outline.md).
A definition has two parts:

- The left side of `:=` is a simple, unquoted name for the new contrast.

- The right side is an expression involving image names previously
  supplied to
  [`images()`](https://michaelhallquist.github.io/ggbrain/reference/images.md).

For example, if
[`images()`](https://michaelhallquist.github.io/ggbrain/reference/images.md)
includes images named `img1` and `img2`, use
`define(zdiff := img1 - img2)`, followed by `geom_brain("zdiff")`.
Standard arithmetic, comparison, and logical operators can be used.
Bracket expressions restrict values to matching voxels, for example
`define(pos := img1[img1 > 2])`.

Categorical conjunctions use a constrained `case_when()` syntax:

    define(overlap := case_when(
      img1 > 2 & img2 > 2 ~ "Both",
      img1 > 2 ~ "Image 1",
      img2 > 2 ~ "Image 2"
    ))

Conditions are evaluated from top to bottom and the first match wins.
Category labels must be quoted strings and become factor levels.
Unmatched voxels remain missing unless a final `TRUE ~ "Other"` clause
is supplied. The legacy semicolon form is still accepted in character
definitions and follows the same first-match precedence.

A single unquoted contrast can be supplied per `define()` call. To
define several contrasts at once, use a named character vector:
`define(c(diff = "img1 - img2", sum = "img1 + img2"))`. The older
character form, `define("diff := img1 - img2")`, also remains supported.

Defining a contrast does not draw it. Add a layer that refers to the
contrast name, such as `geom_brain("diff")`. Defining it separately is
useful when the same contrast is reused or further restricted in
multiple layers.

## Examples

``` r
  # T1-weighted template
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  # signed reward prediction error map
  signed_pe <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # unsigned (absolute value) prediction error map
  abspe <- system.file("extdata", "abspe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # preferred unquoted syntax: symbols on the right refer to names in images()
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    define(signed_gt_abs := signed_pe - abspe) +
    geom_brain("signed_gt_abs")

  # a defined contrast can be restricted when used by a layer
  gg_obj <- ggbrain() +
    images(c(underlay = t1, signed_pe = signed_pe, abspe = abspe)) +
    slices(c("x = 25%", "x = 75%")) +
    define(signed_gt_abs := signed_pe - abspe) +
    geom_brain(
      "signed_gt_abs[signed_gt_abs > 0]",
      fill_scale = ggplot2::scale_fill_distiller("Positive difference", palette = "Reds")
    )

  # categorical conjunction with first-match precedence
  conjunction <- define(pe_type := case_when(
    signed_pe > 3 & abspe > 3 ~ "Both",
    signed_pe > 3 ~ "Signed PE",
    abspe > 3 ~ "Absolute PE"
  ))

  # multiple definitions and legacy string syntax
  several <- define(c(
    difference = "signed_pe - abspe",
    total = "signed_pe + abspe"
  ))
  legacy <- define("difference := signed_pe - abspe")
```
