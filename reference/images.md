# Add images to a ggbrain object

Add images to a ggbrain object

## Usage

``` r
images(images = NULL, volumes = NULL, labels = NULL, filter = NULL)
```

## Arguments

- images:

  a character vector or ggbrain_images object containing NIfTI images to
  add to this plot

- volumes:

  a number indicating the volume within the `images` to display. At
  present, this must be a single number – perhaps in the future, it
  could be a vector so that many timepoints in a 4-D image could be
  displayed.

- labels:

  a data.frame or named list of data.frame objects corresponding to
  images that should be labeled. You can only provide a data.frame if
  there is a single image being added. If multiple images are added, the
  names of the `labels` list are used to align the labels with a given
  matching image.

- filter:

  a named list or character string specifying an expression of values to
  retain in the image, or a numeric vector of values to retain. Calls
  ggbrain_images\$filter_image()

## Value

a `ggb` object with the relevant images and an action of 'add_images'

## Examples

``` r
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  gg_obj <- ggbrain() +
    images(c(underlay = t1))
```
