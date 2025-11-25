# This function counts the number of neighboring/touching pixels in a 2D binary image

This function counts the number of neighboring/touching pixels in a 2D
binary image

## Arguments

- im:

  A boolean matrix representing a binary image

- diagonal:

  Whether to count diagonal elements as valid neighbors

## Value

A matrix of the same size as `im` containing the number of neighboring
pixels

## Details

This is an internal function used by geom_outline to clean up outlines

## Author

Michael Hallquist
