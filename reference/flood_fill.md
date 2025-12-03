# This function flood fills a binary image with TRUE for any value of FALSE

This function flood fills a binary image with TRUE for any value of
FALSE

## Arguments

- im:

  A boolean matrix reference representing a binary image

- x:

  the starting x position for fill

- y:

  the starting y position for fill

- r:

  the number of rows in im

- c:

  the number of columns in im

## Value

Nothing. The matrix `im` is modified in place (by reference)

## Details

This is an internal function used by geom_outline to clean up outlines.
Uses an iterative approach with an explicit stack to avoid stack
overflow on large images.

## Author

Michael Hallquist
