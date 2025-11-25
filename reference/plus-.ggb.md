# addition operator for ggb object to support ggplot-like syntax

addition operator for ggb object to support ggplot-like syntax

## Usage

``` r
# S3 method for class 'ggb'
o1 + o2
```

## Arguments

- o1:

  the first object inheriting the ggb class

- o2:

  the second object inheriting the ggb class

## Value

a modified version of the o1 object with o2 added to it

## Details

Note that the addition operator always clones the underlying o1 object
rather than modifying it in place
