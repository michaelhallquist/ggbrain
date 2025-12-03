# Annotate orientation labels (L/R/A/P/S/I) on each panel using NIfTI header info

Annotate orientation labels (L/R/A/P/S/I) on each panel using NIfTI
header info

## Usage

``` r
annotate_orientation(size = 3, color = "white", offset = 0, ...)
```

## Arguments

- size:

  Text size passed to ggplot2::annotate

- color:

  Text color passed to ggplot2::annotate

- offset:

  Offset (in data units) to nudge labels away from the image edge. Can
  be scalar or length 4 (left, right, bottom, top)

- ...:

  Additional arguments passed to ggplot2::annotate (e.g., fontface)

## Value

a `ggb` object with action "add_annotations"

## Details

Uses RNifti::orientation on the first image in the ggbrain object
(cached in the slices object) to infer axis labels and places them along
the edges of each panel. If orientation cannot be determined, no
annotations are added.
