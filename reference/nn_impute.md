# Imputes missing values in a 2D matrix based on the nearest non-missing neighbors in a given radius

Imputes missing values in a 2D matrix based on the nearest non-missing
neighbors in a given radius

## Arguments

- in_mat:

  a 2D matrix to fill using nearest neighbors

- neighbors:

  the number of closest non-NA neighboring values to return within
  `in_mat`. Default is 4.

- radius:

  the radius (in pixels) around each missing value to search for
  non-missing neighbors. Default is 8.

- aggfun:

  the function used to aggregate the neighbors in imputation. Supports
  "mean", "median", and "mode."

- ignore_zeros:

  if TRUE, then zero is not a valid imputation value (since these are
  not data in NIfTIs)

## Value

A copy of the matrix with NA values imputed by their nearest neighbors

## Details

The "mode" aggfun should only be used when the matrix `in_mat` can be
converted to integers without loss of information (i.e., the data are
integerish values already).
