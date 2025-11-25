# Convert a 3-column data.frame (dim1, dim2, value) to a 2-D matrix

Convert a 3-column data.frame (dim1, dim2, value) to a 2-D matrix

## Arguments

- df:

  A `data.frame` representing a melted 2-D matrix, having columns dim1,
  dim2, and value

- replace_na:

  if not `NULL`, this numeric value will be used to replace any NAs in
  `df` in the resulting matrix. This is useful if downstream code is not
  built to handle missing values.

## Value

The matrix form of the keyed data.frame object

## Details

There is virtually no input validation of `df`. You must pass a
data.frame that has dim1, dim2, and value as columns. Otherwise, it will
not work as expected.

This is a much faster version of the acast function from `reshape2` that
works only on 2-D matrix conversions.
