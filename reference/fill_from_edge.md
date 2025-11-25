# This function finds holes by flood filling TRUE into a 2D binary image, starting from the edge

This function finds holes by flood filling TRUE into a 2D binary image,
starting from the edge

## Arguments

- im:

  A boolean matrix representing a binary image

- nedges:

  An integer specifying how many starting points along the edge to use
  for filling TRUE. The starts are northwest (1), southwest (2),
  southeast (3), and northeast (4). The compute time increases with the
  number of starts.

## Value

A matrix of the same size as `im` containing the number of neighboring
pixels

## Details

This is an internal function used by geom_outline to clean up outlines

## Author

Michael Hallquist
