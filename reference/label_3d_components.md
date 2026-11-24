# Native 3D connected component labeling using iterative component traversal

Native 3D connected component labeling using iterative component
traversal

## Usage

``` r
label_3d_components(mask, nn = 3)
```

## Arguments

- mask:

  A 3D logical array

- nn:

  Neighborhood connectivity (1=6-conn, 2=18-conn, 3=26-conn)

## Value

A 3D integer array with cluster labels (0 = background)
