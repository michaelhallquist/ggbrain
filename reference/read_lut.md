# Read a neuroimaging color lookup table (LUT)

Parses common neuroimaging atlas color lookup tables into a standardized
tibble containing integer region values, text labels, RGB channels, and
hex colors. The returned table can be passed directly to
[`images`](https://michaelhallquist.github.io/ggbrain/reference/images.md)`(..., labels = lut)`
and
[`scale_fill_lut()`](https://michaelhallquist.github.io/ggbrain/reference/scale_fill_lut.md).

## Usage

``` r
read_lut(
  file,
  format = c("auto", "freesurfer", "itksnap", "fsl"),
  drop_zero = TRUE
)
```

## Arguments

- file:

  Path to a text color lookup table file.

- format:

  Character string specifying the LUT format. One of:

  - `"auto"` (default): Detect format automatically from the header and
    first entry.

  - `"freesurfer"`: Whitespace-delimited `index label R G B [A]` (e.g.,
    `FreeSurferColorLUT.txt`, Yeo, or Schaefer LUTs).

  - `"itksnap"`: ITK-SNAP label description format
    `IDX R G B A VIS MSH "Label"`.

  - `"fsl"`: FSL/FSLeyes label LUT format `index R G B label` (with RGB
    values in `[0, 1]` or `[0, 255]` and optional multi-word labels).

- drop_zero:

  Logical; if `TRUE` (default), remove entries with `value == 0`
  (typically background/unknown labels such as `NONE`, `Unknown`, or
  `Clear Label`), since `ggbrain` treats voxel value 0 as transparent
  background.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- `value`:

  Integer ROI/parcel code matching values in the NIfTI atlas.

- `label`:

  Character region or network name.

- `r`, `g`, `b`:

  Integer RGB channel values in `0..255`.

- `color`:

  Hexadecimal RGB color string (e.g., `"#781286"`).

## See also

[`scale_fill_lut()`](https://michaelhallquist.github.io/ggbrain/reference/scale_fill_lut.md),
[`images()`](https://michaelhallquist.github.io/ggbrain/reference/images.md)

## Examples

``` r
lut_file <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")
yeo_lut <- read_lut(lut_file)
yeo_lut
#> # A tibble: 7 × 6
#>   value label           r     g     b color  
#>   <int> <chr>       <int> <int> <int> <chr>  
#> 1     1 7Networks_1   120    18   134 #781286
#> 2     2 7Networks_2    70   130   180 #4682B4
#> 3     3 7Networks_3     0   118    14 #00760E
#> 4     4 7Networks_4   196    58   250 #C43AFA
#> 5     5 7Networks_5   220   248   164 #DCF8A4
#> 6     6 7Networks_6   230   148    34 #E69422
#> 7     7 7Networks_7   205    62    78 #CD3E4E
```
