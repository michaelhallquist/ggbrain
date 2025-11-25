# Function to convert `ggb` object to ggplot/patchwork object

Function to convert `ggb` object to ggplot/patchwork object

## Usage

``` r
render(x, ...)
```

## Arguments

- x:

  optional. A `ggb` object to be rendered into a
  `ggbrain_patchwork object`.

- ...:

  additional arguments passed to the \$render method of `x`.

## Value

a `ggbrain_patchwork` object of the rendered ggbrain plot

## Details

If no `x` argument is passed in, this function can be used in a ggbrain
addition chain to render a plot to a ggplot-friendly object before
additional ggplot or patchwork calls are added such as `theme()`.

Or if `x` is passed in as an argument, return the rendered plot as a
`ggbrain_patchwork` object.

## Examples

``` r
  t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  
  # version where render is added to the object in a ggplot-style chain
  gg_obj <- ggbrain() +
    images(c(underlay = t1)) + 
    slices(c("x = 25%", "x = 75%")) +
    geom_brain("underlay") + 
    render() + # convert to ggplot-friendly object
    ggplot2::theme(text=ggplot2::element_text(family="Serif"))
    
 # version where a ggbrain object is created in one step, then rendered in another
 brain_obj <- ggbrain() +
    images(c(underlay = t1)) + 
    slices(c("x = 25%", "x = 75%")) +
    geom_brain("underlay")
    
 gg_obj <- render(brain_obj) + patchwork::plot_annotation(title="Overall title")
 
```
