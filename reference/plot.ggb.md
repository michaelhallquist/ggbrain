# S3 method to allow for plot(x) syntax with ggbrain (ggb) objects

S3 method to allow for plot(x) syntax with ggbrain (ggb) objects

## Usage

``` r
# S3 method for class 'ggb'
plot(x, ...)
```

## Arguments

- x:

  the `ggb` object to be plotted

- ...:

  additional arguments passed to the plot method

## Value

The rendered ggbrain_patchwork object, invisibly, or NULL when there are
no layers.

## Details

This will plot the ggbrain object to the current graphics device
