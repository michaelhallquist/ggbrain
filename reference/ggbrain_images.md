# R6 class for compiling images to render in ggplot

R6 class for compiling images to render in ggplot

R6 class for compiling images to render in ggplot

## Value

a `ggbrain_images` R6 class containing fields related to a set of NIfTI
images imported into R

## Details

Note that this class is exported only for power users and rarely needs
to be called directly in typical use of the package. Instead, look at
images().

## Active bindings

- `zero_tol`:

  the (positive) numeric value that should be treated as
  indistinguishable from zero. This value is used to set small values in
  the images to exactly zero for proper masking. Default 1e-6

- `slices`:

  a character vector of cached slice specifications to be used in
  \$get_slices()

- `contrasts`:

  a character vector of cached contrast specifications to be used in
  \$get_slices()

## Methods

### Public methods

- [`ggbrain_images$new()`](#method-ggbrain_images-new)

- [`ggbrain_images$add()`](#method-ggbrain_images-add)

- [`ggbrain_images$add_labels()`](#method-ggbrain_images-add_labels)

- [`ggbrain_images$add_images()`](#method-ggbrain_images-add_images)

- [`ggbrain_images$filter_images()`](#method-ggbrain_images-filter_images)

- [`ggbrain_images$dim()`](#method-ggbrain_images-dim)

- [`ggbrain_images$get_image_names()`](#method-ggbrain_images-get_image_names)

- [`ggbrain_images$get_images()`](#method-ggbrain_images-get_images)

- [`ggbrain_images$get_headers()`](#method-ggbrain_images-get_headers)

- [`ggbrain_images$remove_images()`](#method-ggbrain_images-remove_images)

- [`ggbrain_images$winsorize_images()`](#method-ggbrain_images-winsorize_images)

- [`ggbrain_images$na_images()`](#method-ggbrain_images-na_images)

- [`ggbrain_images$summary()`](#method-ggbrain_images-summary)

- [`ggbrain_images$get_nz_indices()`](#method-ggbrain_images-get_nz_indices)

- [`ggbrain_images$add_slices()`](#method-ggbrain_images-add_slices)

- [`ggbrain_images$add_contrasts()`](#method-ggbrain_images-add_contrasts)

- [`ggbrain_images$reset_slices()`](#method-ggbrain_images-reset_slices)

- [`ggbrain_images$get_slices()`](#method-ggbrain_images-get_slices)

- [`ggbrain_images$get_slices_inplane()`](#method-ggbrain_images-get_slices_inplane)

- [`ggbrain_images$get_labels()`](#method-ggbrain_images-get_labels)

- [`ggbrain_images$get_orientation_labels()`](#method-ggbrain_images-get_orientation_labels)

- [`ggbrain_images$lookup_slices()`](#method-ggbrain_images-lookup_slices)

- [`ggbrain_images$clone()`](#method-ggbrain_images-clone)

------------------------------------------------------------------------

### Method `new()`

create ggbrain_images object consisting of one or more NIfTI images

#### Usage

    ggbrain_images$new(images = NULL, volumes = NULL, labels = NULL, filter = NULL)

#### Arguments

- `images`:

  a character vector of file names containing NIfTI images to read

- `volumes`:

  the volumes to be read from each element of `images`. By default, this
  is 1, in which case the first volume is used, which is appropriate for
  all 3-D images. For 4-D images, `volumes` gives you more flexibility
  over the volume to display.

- `labels`:

  A named list of data.frames with labels that map to values in the
  integer-valued/atlas elements of `images`. If a single data.frame is
  passed, it will be accepted if only a single image is passed, too.
  These are then assumed to correspond

- `filter`:

  A named list of filter expressions to be applied to particular images.
  The names of the list correspond to the names of the `images`
  provided. Each element of the list can either be a character vector
  denoting a filtering expression (e.g., `'value < 100'`) or a numeric
  vector denoting values of the image that should be retained (e.g.,
  `c(5, 10, 12)`).

------------------------------------------------------------------------

### Method `add()`

method to add another ggbrain_images object to this one

#### Usage

    ggbrain_images$add(obj)

#### Arguments

- `obj`:

  the ggbrain_images object to combine with this one

------------------------------------------------------------------------

### Method `add_labels()`

add a labels data.frame that connects an integer-valued image with a set
of labels

#### Usage

    ggbrain_images$add_labels(...)

#### Arguments

- `...`:

  named arguments containing data.frame objects for each image to be
  labeled. The argument name should match the image name to be labeled
  and the value should be a data.frame containing `value` and `label`.

#### Details

As a result of \$add_labels, the \$get_slices method will always remap
the numeric values for label images to the corresponding text-based
labels in the label data. In addition, a new attribute will be returned
called "slice_labels" that contains a row for each region represented in
each slice.

------------------------------------------------------------------------

### Method `add_images()`

add one or more images to this ggbrain_images object

#### Usage

    ggbrain_images$add_images(images = NULL, volumes = NULL)

#### Arguments

- `images`:

  a character vector of file names containing NIfTI images to read

- `volumes`:

  a number indicating the volume within the `images` to read. At
  present, this must be a single number – perhaps in the future, it
  could be a vector so that many timepoints in a 4-D image could be
  displayed.

------------------------------------------------------------------------

### Method `filter_images()`

filters an image based on an expression such as a subsetting operation

#### Usage

    ggbrain_images$filter_images(filter = NULL)

#### Arguments

- `filter`:

  a character string or numeric vector of the filter to apply

#### Details

if expr is a numeric vector, only values in this set will be retained.
If a character string expression is used, it should use the variable
name `'value'` to refer to the numeric values to be filtered, such as
`'value > 10'`.

------------------------------------------------------------------------

### Method [`dim()`](https://rdrr.io/r/base/dim.html)

return the 3D dimensions of the images contained in this object

#### Usage

    ggbrain_images$dim()

------------------------------------------------------------------------

### Method `get_image_names()`

return the names of the images contained in this object

#### Usage

    ggbrain_images$get_image_names()

------------------------------------------------------------------------

### Method `get_images()`

return the RNifti objects of one or more images contained in this object

#### Usage

    ggbrain_images$get_images(img_names = NULL, drop = TRUE)

#### Arguments

- `img_names`:

  The names of images to return. Use `$get_image_names()` if you're
  uncertain about what is available.

- `drop`:

  If TRUE, a single image is returned as an RNifti object, rather than a
  single-element list containing that object.

------------------------------------------------------------------------

### Method `get_headers()`

return the NIfTI headers for one or more images contained in this object

#### Usage

    ggbrain_images$get_headers(img_names = NULL, drop = TRUE)

#### Arguments

- `img_names`:

  The names of images whose header are returned. Use
  `$get_image_names()` if you're uncertain about what is available.

- `drop`:

  If TRUE, a single header is returned as an niftiHeader object, rather
  than a single-element list containing that object.

------------------------------------------------------------------------

### Method `remove_images()`

method for removing one or more images from the ggbrain_images object

#### Usage

    ggbrain_images$remove_images(img_names)

#### Arguments

- `img_names`:

  names of images to remove from object

------------------------------------------------------------------------

### Method `winsorize_images()`

winsorize the tails of a set of images to pull in extreme values

#### Usage

    ggbrain_images$winsorize_images(img_names, quantiles = c(0.001, 0.999))

#### Arguments

- `img_names`:

  The names of images in the ggbrain_images object to be winsorized

- `quantiles`:

  The lower and upper quantiles used to define the thresholds for
  winsorizing.

------------------------------------------------------------------------

### Method `na_images()`

method to set values less than `threshold` to NA

#### Usage

    ggbrain_images$na_images(img_names, threshold = NULL)

#### Arguments

- `img_names`:

  The names of images in the ggbrain_images object whose values should
  be set to NA

- `threshold`:

  The threshold value whose absolute value used to determine which
  voxels to set to NA. If `NULL`, use the pvt_zero_tol field (default
  1e-6).

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

print a summary of the ggbrain_images object

#### Usage

    ggbrain_images$summary()

------------------------------------------------------------------------

### Method `get_nz_indices()`

return the indices of non-zero voxels

#### Usage

    ggbrain_images$get_nz_indices(img_names = NULL)

#### Arguments

- `img_names`:

  The names of images in the ggbrain_images object whose non-zero
  indices should be looked up

#### Details

Note that this function looks for non-zero voxels in any of the images
specified by `img_names`.

------------------------------------------------------------------------

### Method `add_slices()`

adds one or more slices to the cached slices that will be retrieved by
\$get_slices() when no `slices` argument is passed.

#### Usage

    ggbrain_images$add_slices(slices = NULL)

#### Arguments

- `slices`:

  a character vector containing one or more slices to be extracted by
  `$get_slices`. Uses the syntax `"<xyz>=<number>"`. Example:
  `c("x=10", "y=50%")`

------------------------------------------------------------------------

### Method `add_contrasts()`

adds one or more contrasts to the cached contrasts that will be
retrieved by \$get_slices() when no `contrasts` argument is passed.

#### Usage

    ggbrain_images$add_contrasts(contrasts = NULL)

#### Arguments

- `contrasts`:

  a character vector containing one or more contrasts to be extracted by
  `$get_slices`. Uses the syntax
  `"<img_name>[subset_expression] + <img_name>"`.

------------------------------------------------------------------------

### Method `reset_slices()`

remove all cached slice settings

#### Usage

    ggbrain_images$reset_slices()

------------------------------------------------------------------------

### Method `get_slices()`

get slice data for one or more slices based on their coordinates

#### Usage

    ggbrain_images$get_slices(
      slices = NULL,
      img_names = NULL,
      contrasts = NULL,
      fill_labels = FALSE,
      make_square = TRUE,
      remove_null_space = TRUE
    )

#### Arguments

- `slices`:

  a vector of slice positions

- `img_names`:

  a character vector of images contained in the ggbrain_images object to
  be sliced

- `contrasts`:

  a named character vector of contrasts to be calculated for each slice

- `fill_labels`:

  if TRUE, the numeric value of the image will be used for any value
  that does not have a corresponding label in the labels data.frame.
  Default: FALSE

- `make_square`:

  If TRUE, make all images square and of the same size

- `remove_null_space`:

  If TRUE, remove slices where all values are approximately zero

#### Details

This function always returns a data.frame where each row represents a
slice requested by the user. The \$slice_data element is a list-column
where each element is itself a list of slice data for a given
layer/image (e.g., underlay or overlay) . The \$slice_matrix is a
list-column where each element is a list of 2-D matrices, one per
layer/image. @return a ggbrain_slices object containing the requested
slices and contrasts

------------------------------------------------------------------------

### Method `get_slices_inplane()`

get_slices_inplane is mostly an internal funciton for getting one or
more slices from a given plane

#### Usage

    ggbrain_images$get_slices_inplane(
      imgs = NULL,
      slice_numbers,
      plane,
      drop = FALSE
    )

#### Arguments

- `imgs`:

  The names of images to slice

- `slice_numbers`:

  The numbers of slices in the specified plant to grab

- `plane`:

  The image plane to slice. Must be "coronal", "sagittal", or "axial"

- `drop`:

  if TRUE, a single slice is returned as a 2D matrix instead of a 3D
  matrix with a singleton first dimension

#### Returns

A 3D matrix of slices x dim1 x dim2

------------------------------------------------------------------------

### Method `get_labels()`

return a list of data.frames containing labels for a given image

#### Usage

    ggbrain_images$get_labels()

#### Details

the names of the list correspond directly with the names of the images

------------------------------------------------------------------------

### Method `get_orientation_labels()`

internal helper to determine anatomical orientation labels for the first
image

#### Usage

    ggbrain_images$get_orientation_labels()

#### Returns

a list with elements x, y, z giving the orientation codes (e.g.,
c(\\L\\, \\R\\))

------------------------------------------------------------------------

### Method `lookup_slices()`

internal function to lookup which slices to display along each axis
based on their quantile, xyz coordinate, or ijk coordinate

#### Usage

    ggbrain_images$lookup_slices(slices, ignore_null_space = TRUE)

#### Arguments

- `slices`:

  A character vector of coordinates for slices to display

- `ignore_null_space`:

  If TRUE, any coordinates specified as quantiles (e.g., x = 50%) use
  the quantiles of only the non-zero slices (ignoring blank sliaces)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ggbrain_images$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
