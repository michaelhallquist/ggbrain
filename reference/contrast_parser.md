# helper function to calculate contrasts of one or more images using a combination of image arithmetic and logical subsetting

helper function to calculate contrasts of one or more images using a
combination of image arithmetic and logical subsetting

## Usage

``` r
contrast_parser(expr, data = NULL, default_val = NA_real_)
```

## Arguments

- expr:

  a string or expression containing the image calculation to be
  performed

- data:

  a data.frame containing all variables used in `expr`. This will be
  used to perform contrast calculations

- default_val:

  the value to be returned for any element of the contrast calculation
  that does not pass through the arithmetic, either because of logical
  subsetting or because it is exactly zero. In general, leave this as
  `NA_real_` unless you know what you're doing.

## Author

Michael Hallquist
