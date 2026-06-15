# Evaluate an image or numeric/logical contrast over complete 3D volumes

Evaluate an image or numeric/logical contrast over complete 3D volumes

## Usage

``` r
evaluate_definition_3d(img_obj, definition, contrasts = NULL)
```

## Arguments

- img_obj:

  A ggbrain_images object.

- definition:

  A numeric or logical image expression.

- contrasts:

  Optional named character contrast definitions available to the
  expression.

## Value

A list with values, mask, and reference_image.
