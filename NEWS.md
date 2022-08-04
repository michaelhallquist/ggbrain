# ggbrain 0.5.2 (development)

* feature: montage() function to make adding many slices in a plane easy
* feature: support plot() on ggbrain() objects so that the ggbrain settings can be amended prior to the render and plot steps
* feature: support overall plot aesthetics including title, base_size, bg_color, and text_color. These are arguments to ggbrain()
* feature: set tiny values on each slice to NA to support transparency at image boundaries (e.g., in 'dead space' on a structural)
* feature: add_slice() function adds single slice and supports panel-specific aesthetics such as title and text_color
* bugfix:  correct accidental resetting of unified scale limits when a bisided scale was empty on one side
* bugfix:  correct range_breaks() for empty data
* bugfix:  pass through breaks to layers regardless of whether scale or breaks are set first

# ggbrain 0.5.1 (2Aug2022)

* abstract geom_brain and geom_outline to different R6 classes that inherit from ggbrain_layer
* support blur_edge to allow for blurred outlines that are less jagged
* support clean_specks = <number> to remove regions of this size or smaller from slices
* support alpha transparency in geom_brain and geom_outline layers with `alpha=<number>` syntax
* support control of how outlines are grouped/calculated in labeled images using `geom_outline(mapping=aes(group=<label_col>))`
* bugfix: allow for empty annotations in overall plot

# ggbrain 0.5 (29Jul2022)

* Initial internal release of ggbrain package