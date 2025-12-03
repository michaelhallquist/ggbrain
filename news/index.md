# Changelog

## ggbrain 0.9.2 (3Dec2025)

- feature: added
  [`annotate_orientation()`](https://michaelhallquist.github.io/ggbrain/reference/annotate_orientation.md)
  function to add anatomical orientation labels to plots
- bugfix: avoid double calls to `refine_image()` when unifying
  categorical scales by releveling before data assignment
- bugfix: ensure `fill_holes` uses mode for categorical/labeled data
  (even with fixed outlines) and cap imputation radius for small slices
- bugfix: replace deprecated `aes_string()` with tidy-eval `aes()`
  mappings for rasters and labels
- bugfix: replace deprecated
  [`rlang::flatten()`](https://rlang.r-lib.org/reference/flatten.html)
  with internal `flatten_list_once()` helper
- bugfix: correct row/column swap in the eastward check of
  count_neighbors.
- bugfix: switched `flood_fill` to stack-based approach to avoid
  overflow due to recursion.
- bugfix: Correct invalid 1-based indexing assumption in `nearest_pts`
  used for filling holes.
- feature: Improve speed of `find_threads` by only decrementing neighbor
  counts for pixels identified from the initial full scan.
- tests: add core vignette-style integration tests covering rendering,
  outlines, and labeled atlas slices; add tests for `refine_image` call
  count and hole filling

## ggbrain 0.9.1 (22Aug2025)

CRAN release: 2025-08-22

- updated ggplot_add methods to be compatible with ggplot2 4.0

## ggbrain 0.9.0 (18Mar2025)

CRAN release: 2025-03-20

- notice: annotate_panel is now called annotate_slice for consistency
- notice: the plot() method now works with rendered and un-rendered
  objects, always drawing to the current graphics device
- feature: Support use of label columns in contrast specification
- feature: Support use of inline factor() specification in aes() for a
  `geom_outline` or `geom_brain` layer
- feature: If layer definition uses := syntax, use the lefthand side as
  the layer name when no name is provided
- bugfix: Allow for quantile “q” specifications for label position that
  can be 1-100 (percentile) or 0-1 (quantile)
- bugfix: Do not throw error when contrast specification includes equals
  sign (logical tests)
- bugfix: Only use := syntax for naming contrasts. For compound contrast
  expressions, use = ; =
- bugfix: Pass through label_columns as attribute from inline contrast
  definitions of the form =
- bugfix: Handle fixed fill in ggbrain_layers when alpha \< 1
- bugfix: retain the order of ordered factors in labels when
  `unify_scales = TRUE`
- bugfix: copy through label columns for simple subset contrasts
- bugfix: extend background color rectangle to cover entire plot when
  legend is large
- bugfix: support ggplot2 3.5.0+, working around collision with
  ggnewscale for categorical images

## ggbrain 0.8.1 (21Mar2023)

CRAN release: 2023-03-21

- feature: `ggbrain` objects can be added together, allowing common
  parts across plots to be reused
- feature: `geom_outline` now supports dilation and erosion using the
  `dil_ero` argument
- feature: `geom_region_label*` and `geom_region_text*` now support a
  `min_px` argument that controls the minimum number of pixels on a
  slice that will result in a label. Helps reduce labeling of small
  regions
- feature: support aes(outline=, group=) for geom_outline, allowing for
  subdivision of outlines
- bugfix: add workaround for show.legend bug when there are multiple
  fill layers: <https://github.com/eliocamp/ggnewscale/issues/32>
- bugfix: switch to linewidth instead of size for borders in
  ggbrain_panel object
- bugfix: default to cyan color (with message) if geom_outline is added
  without any color information
- bugfix: correction to pass forward data.frame after `fill_holes`
  algorithm is applied
- bugfix: nn interpolation for `fill_holes` now takes the mean of
  neighbors for continuous-valued data
- bugfix: use the correct fill column when using `remove_specks` for a
  categorical image

## ggbrain 0.8 (21Oct2022)

- first public release of the package to CRAN (hooray!)
- feature: add
  [`define()`](https://michaelhallquist.github.io/ggbrain/reference/define.md)
  function to support contrast definition in ggbrain pipeline outside of
  a given layer
- feature: fixed fill colors are now supported in `geom_brain` using
  `geom_brain(fill='color_here')`
- feature:
  [`images()`](https://michaelhallquist.github.io/ggbrain/reference/images.md)
  supports a `volumes` argument so that volumes from 4-D images can be
  visualized
- feature: [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on a
  ggbrain object supports `guides` argument that passes to
  `plot_layout()` in `patchwork`. Also, the default is now
  `guides="collect"`, which combines shared legends.
- feature: support `min_coord` and `max_coord` in `montage` function for
  coordinate-based ranges
- update: archive ggbrain_legacy and remove cowplot dependency
- update: `add_images()` is now
  [`images()`](https://michaelhallquist.github.io/ggbrain/reference/images.md)
  for nonredundancy
- update: `add_slices()` is now
  [`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md)
  for nonredundancy
- update: renamed `outline_size` to just `size` in `geom_outline` for
  consistency with ggplot2 logic (e.g., `geom_contour`).
- update: removed `add_slice()` in favor of
  [`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md).
  If attributes such as `bg_color` are set by
  [`slices()`](https://michaelhallquist.github.io/ggbrain/reference/slices.md),
  these will be the same for each slice in the operation.
- bugfix: properly display `geom_outline` for continuous images

## ggbrain 0.7 (23Sep2022)

- feature: abstract `fill_holes`, `trim_threads`, and `remove_specks` to
  `ggbrain_layer` – exposed in `geom_brain` and `geom_outline`. This
  allows these modifications to be made for contrast layers.
- feature: fill_holes now supported in `geom_brain` and `geom_outline`
  objects. Uses flood fill algorithm to find interior holes, then uses
  nearest neighbor imputation, taking the mode for categorical images
  and mean for continuous images.
- feature: for annotations, q syntax now allows for quantiles above 1 or
  below 0, where 2 = 200% of max and -1 is -100% of min. This allows for
  annotations to be placed outside of image boundaries.
- feature: implement Rcpp-based flood fill algorithm for finding
  interior holes on slices. Used for fill_holes.
- feature: implement Rcpp-based pixel neighbor counting algorithm to
  clean up frayed edges of outlines and fills.
- feature: switch to Rcpp mat2df function instead of reshape2::melt for
  speed. Remove reshape2 dependency
- bugfix: when geom_raster is provided with alpha and
  na.value=‘transparent’, it draws semi-transparent squares. Switched to
  na.omit() when alpha \< 1.
- bugfix: ggbrain_slices still returns data for layers that are all NA
  so that layers are not dropped from range calculations.
- bugfix: do not add a ggbrain_layer to a plot if all of its values are
  NA. Adds all_na field.
- bugfix: properly handle ggbrain_images filter when an integer vector
  is provided
- bugfix: fix simple_subset for contrast_parser when there are no
  brackets in the expression
- bugfix: correct contrast_parser for conjunctions so that both numeric
  value and label are returned. Also now use := syntax for values.
- bugfix: fully remove assumption of “label” column in categorical
  layers, fixing calculation of unique values in each categorical layer
- bugfix: convert NAs to 0s within contrast parsing so that numeric
  logical comparisons always yield TRUE/FALSE and not NA.

## ggbrain 0.6 (9Aug2022)

- feature: for labeled images, clean_specks now counts specks by
  ROI/mask value
- feature: support filter expression in add_image() to filter (subset)
  the data for a given image. Useful for thresholding or for subsetting
  an atlas to retain only certain values.
- feature: montage() function to make adding many slices in a plane easy
- feature: support plot() on ggbrain() objects so that the ggbrain
  settings can be amended prior to the render and plot steps
- feature: support overall plot aesthetics including title, base_size,
  bg_color, and text_color. These are arguments to ggbrain()
- feature: set tiny values on each slice to NA to support transparency
  at image boundaries (e.g., in ‘dead space’ on a structural)
- feature: add_slice() function adds single slice and supports
  panel-specific aesthetics such as title and text_color
- feature: contrasts that are simple subsetting operation on a labeled
  image retain labels in the contrast data
- bugfix: correct accidental resetting of unified scale limits when a
  bisided scale was empty on one side
- bugfix: correct range_breaks() for empty data
- bugfix: pass through breaks to layers regardless of whether scale or
  breaks are set first
- bugfix: cleaning specks no longer fails when a slice is empty
- bugfix: refactor outlines so that empty slices do not generate
  completely filled layer

## ggbrain 0.5.1 (2Aug2022)

- abstract geom_brain and geom_outline to different R6 classes that
  inherit from ggbrain_layer
- support blur_edge to allow for blurred outlines that are less jagged
- support clean_specks = to remove regions of this size or smaller from
  slices
- support alpha transparency in geom_brain and geom_outline layers with
  `alpha=<number>` syntax
- support control of how outlines are grouped/calculated in labeled
  images using `geom_outline(mapping=aes(group=<label_col>))`
- bugfix: allow for empty annotations in overall plot

## ggbrain 0.5 (29Jul2022)

- Initial internal release of ggbrain package
