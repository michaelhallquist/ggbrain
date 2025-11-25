# Variant of geom_text_repel used for plotting region labels on slices with separation from other labels

Variant of geom_text_repel used for plotting region labels on slices
with separation from other labels

## Usage

``` r
geom_region_text_repel(image, label_column = "label", min_px = 1L, ...)
```

## Arguments

- image:

  The name of the image within the underlying ggbrain_slices object that
  contains the labeled data positions

- label_column:

  The column name name for the labels to use within the slice data

- min_px:

  The minimum number of pixels present on a slice that will result in a
  text label. Default: 1

- ...:

  All other parameters passed through to geom_text_repel

## Value

a `ggb` object with the relevant ggbrain_label field and an action of
"add_region_labels"
