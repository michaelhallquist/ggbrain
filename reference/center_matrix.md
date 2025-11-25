# helper function to center a numeric matrix within a (larger) target matrix size

helper function to center a numeric matrix within a (larger) target
matrix size

## Usage

``` r
center_matrix(output_dim, mat, default_value = NA_real_, drop_zeros = TRUE)
```

## Arguments

- output_dim:

  the desired dimensions of the output matrix

- mat:

  the 2D numeric matrix containing values to be centered in the output
  matrix

- default_value:

  the value that should fill padded rows and columns of the output
  matrix

- drop_zeros:

  if TRUE, all zero-valued rows and columns of `mat` will be dropped
  before the data are centered within the output matrix. This is useful
  if the matrix is asymmetric, but you still want to have it be
  dead-center in the output.

## Value

an expanded matrix of size `output_dim` with the input matrix `mat`
centered within it.
