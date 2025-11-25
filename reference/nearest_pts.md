# Finds the nearest non-missing neighbors of a target point in a 2D matrix

Finds the nearest non-missing neighbors of a target point in a 2D matrix

## Arguments

- x:

  x-position of the point whose neighbors should be found within
  `in_mat`

- y:

  y-position of the point whose neighbors should be found within
  `in_mat`

- in_mat:

  a 2D matrix to search for neighbors of `pt`

- neighbors:

  the number of closest non-NA neighboring values to return within
  `in_mat`

- radius:

  the radius around `pt` to search. Default: 8.

- ignore_zeros:

  if TRUE, then zero is not a valid imputation value (since these are
  not data in NIfTIs)

## Value

A vector of `neighbors` closest neighboring values around `pt`
