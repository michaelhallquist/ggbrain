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

## Details

Categorical conjunctions can be written using
`case_when(<logical expression> ~ "<label>", ...)`, where the first
matching clause takes precedence. Unmatched values remain missing; a
final `TRUE ~ "Other"` clause can provide a fallback. The legacy
semicolon-separated `<label> = <logical expression>` syntax is also
supported. Both forms use first-match precedence, retain integer codes
in `value` for image processing, and return the user labels as factor
levels in `label`.

## Author

Michael Hallquist
