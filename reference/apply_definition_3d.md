# Apply a definition/contrast string to a 3D image volume

Apply a definition/contrast string to a 3D image volume

## Usage

``` r
apply_definition_3d(img_obj, definition)
```

## Arguments

- img_obj:

  A ggbrain_images object

- definition:

  A definition string, e.g., a simple image name or a filtered
  expression

## Value

A 3D array with the contrast applied (masked/thresholded volume)
