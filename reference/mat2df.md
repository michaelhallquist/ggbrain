# Fast conversion of 2D mat to 3-column data.frame with dim1, dim2, value

Converts a 2D numeric matrix into a 3-column data.frame

## Arguments

- mat:

  A `matrix` to convert to data.frame

- na_zeros:

  If true, convert values with absolute magnitude below 1e-4 to missing
  values.

## Value

A 3-column data.frame with dim1, dim2, and value

## Details

This function is a faster version of reshape2::melt for the simple 2-D
case. It is about 2.5x faster than melt.

## Author

Michael Hallquist
