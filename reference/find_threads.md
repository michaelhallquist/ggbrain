# This function finds 'threads' hanging off of the edges of blobs in an image, allowing the user to trim them

This function finds 'threads' hanging off of the edges of blobs in an
image, allowing the user to trim them

## Arguments

- im:

  A numeric matrix representing an image, with non-zero values
  representing pixels to display

- min_neighbors:

  the minimum number of neighbors a pixel must have to be retained

- maxit:

  the maximum number of iterations to run the thread trimming algorithm.
  Default: 15.

- diagonal:

  Whether to count diagonal elements as valid neighbors

## Value

A logical matrix matrix of the same size as `im` containing the number
of neighboring pixels

## Details

This algorithm runs count_neighbors iteratively until no pixel exceeds
the trimming threshold `min_neighbors` or the maximum number of
iterations, `maxit`, is reached.

By running iteratively, long tails are trimmed sequentially by pruning
the most disconnected voxels.

The algorithm computes neighbor counts once initially, then uses
incremental updates when pixels are removed. This avoids redundant
full-matrix scans on each iteration, providing significant speedup for
large images.

## Author

Michael Hallquist
