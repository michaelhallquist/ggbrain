# S3 method to allow for plot() syntax with rendered ggbrain patchwork objects

S3 method to allow for plot() syntax with rendered ggbrain patchwork
objects

default S3 method for ggbrain_patchwork objects (post-render)

## Usage

``` r
# S3 method for class 'ggbrain_patchwork'
plot(x, ...)

# S3 method for class 'ggbrain_patchwork'
print(x, ...)
```

## Arguments

- x:

  the `ggbrain_patchwork` object to be plotted

- ...:

  additional arguments. Not currently used

## Value

`patchworkGrob` object, invisibly
