# ggbrain 0.7 (23Sep2022)
* feature: abstract fill_holes, trim_threads, and remove_specks to ggbrain_layer -- exposed in geom_brain and geom_outline. This allows these modifications
    to be made for contrast layers.
* feature: fill_holes now supported in add_images. Uses flood fill algorithm to find interior holes, then uses nearest neighbor imputation,
    taking the mode for categorical images and mean for continuous images.
* feature: for annotations, q<number> syntax now allows for quantiles above 1 or below 0, where 2 = 200% of max and -1 is -100% of min.
    This allows for annotations to be placed outside of image boundaries.
* feature: implement Rcpp-based flood fill algorithm for finding interior holes on slices. Used for fill_holes.
* feature: implement Rcpp-based pixel neighbor counting algorithm to clean up frayed edges of outlines and fills.
* feature: switch to Rcpp mat2df function instead of reshape2::melt for speed. Remove reshape2 dependency
* bugfix:  when geom_raster is provided with alpha and na.value='transparent', it draws semi-transparent squares. Switched to na.omit() when alpha < 1.
* bugfix:  ggbrain_slices still returns data for layers that are all NA so that layers are not dropped from range calculations.
* bugfix:  do not add a ggbrain_layer to a plot if all of its values are NA. Adds all_na field.
* bugfix:  properly handle ggbrain_images filter when an integer vector is provided
* bugfix:  fix simple_subset for contrast_parser when there are no brackets in the expression
* bugfix:  correct contrast_parser for conjunctions so that both numeric value and label are returned. Also now use := syntax for values.
* bugfix:  fully remove assumption of "label" column in categorical layers, fixing calculation of unique values in each categorical layer
* bugfix:  convert NAs to 0s within contrast parsing so that numeric logical comparisons always yield TRUE/FALSE and not NA.

# ggbrain 0.6 (9Aug2022)

* feature: for labeled images, clean_specks now counts specks by ROI/mask value
* feature: support filter expression in add_image() to filter (subset) the data for a given image. Useful for thresholding
    or for subsetting an atlas to retain only certain values.
* feature: montage() function to make adding many slices in a plane easy
* feature: support plot() on ggbrain() objects so that the ggbrain settings can be amended prior to the render and plot steps
* feature: support overall plot aesthetics including title, base_size, bg_color, and text_color. These are arguments to ggbrain()
* feature: set tiny values on each slice to NA to support transparency at image boundaries (e.g., in 'dead space' on a structural)
* feature: add_slice() function adds single slice and supports panel-specific aesthetics such as title and text_color
* feature: contrasts that are simple subsetting operation on a labeled image retain labels in the contrast data
* bugfix:  correct accidental resetting of unified scale limits when a bisided scale was empty on one side
* bugfix:  correct range_breaks() for empty data
* bugfix:  pass through breaks to layers regardless of whether scale or breaks are set first
* bugfix:  cleaning specks no longer fails when a slice is empty
* bugfix:  refactor outlines so that empty slices do not generate completely filled layer

# ggbrain 0.5.1 (2Aug2022)

* abstract geom_brain and geom_outline to different R6 classes that inherit from ggbrain_layer
* support blur_edge to allow for blurred outlines that are less jagged
* support clean_specks = <number> to remove regions of this size or smaller from slices
* support alpha transparency in geom_brain and geom_outline layers with `alpha=<number>` syntax
* support control of how outlines are grouped/calculated in labeled images using `geom_outline(mapping=aes(group=<label_col>))`
* bugfix: allow for empty annotations in overall plot

# ggbrain 0.5 (29Jul2022)

* Initial internal release of ggbrain package