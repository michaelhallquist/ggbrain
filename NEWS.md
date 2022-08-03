# ggbrain 0.5.2

* added montage() function to make adding many slices in a plane easy

# ggbrain 0.5.1 (2Aug2022)

* abstract geom_brain and geom_outline to different R6 classes that inherit from ggbrain_layer
* support blur_edge to allow for blurred outlines that are less jagged
* support clean_specks = <number> to remove regions of this size or smaller from slices
* support alpha transparency in geom_brain and geom_outline layers with `alpha=<number>` syntax
* support control of how outlines are grouped/calculated in labeled images using `geom_outline(mapping=aes(group=<label_col>))`
* bugfix: allow for empty annotations in overall plot

# ggbrain 0.5 (29Jul2022)

* Initial internal release of ggbrain package