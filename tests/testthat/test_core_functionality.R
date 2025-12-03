test_that("basic ggbrain pipeline renders a patchwork object with expected layers", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  ggb_obj <- ggbrain(bg_color = "gray90", text_color = "black") +
    images(c(underlay = underlay, overlay = overlay)) +
    slices("z=0") +
    geom_brain("underlay", name = "underlay", show_legend = FALSE) +
    geom_brain("overlay[overlay > 3]", name = "overlay_thr", show_legend = FALSE) +
    geom_outline("overlay[overlay > 3]", name = "overlay_outline", outline = "cyan", size = 1L) +
    annotate_coordinates()

  patch <- ggb_obj$render(guides = "collect")
  expect_s3_class(patch, "ggbrain_patchwork")

  panels <- ggb_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  expect_length(panels, 1)

  lay_names <- panels[[1]]$get_layer_names()
  expect_setequal(lay_names, c("underlay", "overlay_thr", "overlay_outline"))
})

test_that("categorical labels stay unified across slices and render", {
  skip_on_cran()
  library(ggplot2)

  atlas_img <- system.file("extdata", "Schaefer_200_7networks_2009c_2mm.nii.gz", package = "ggbrain")
  atlas_labels <- read.csv(system.file("extdata", "Schaefer_200_7networks_labels.csv", package = "ggbrain"))
  atlas_labels$value <- atlas_labels$roi_num

  ggb_obj <- ggbrain(bg_color = "gray90", text_color = "black") +
    images(c(atlas = atlas_img), labels = list(atlas = atlas_labels)) +
    slices(c("z=20", "z=40")) +
    geom_brain(definition = "atlas", name = "atlas_fill", mapping = aes(fill = network), show_legend = FALSE, unify_scales = TRUE) +
    geom_outline(definition = "atlas", name = "atlas_outline", mapping = aes(group = network), outline = "black", show_legend = FALSE) +
    annotate_coordinates()

  patch <- ggb_obj$render(guides = "collect")
  expect_s3_class(patch, "ggbrain_patchwork")

  panels <- ggb_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  expect_length(panels, 2)

  lay1 <- panels[[1]]$get_layers()[[1]]$data
  lay2 <- panels[[2]]$get_layers()[[1]]$data
  expect_identical(levels(lay1$network), levels(lay2$network))
  expect_true(length(levels(lay1$network)) >= 1)
})

test_that("annotate_orientation adds labels when orientation is available", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")

  ggb_obj <- ggbrain(bg_color = "gray90", text_color = "black") +
    images(c(underlay = underlay)) +
    slices(c("z=0", "x=0", "y=0")) +
    geom_brain("underlay", show_legend = FALSE) +
    annotate_orientation(color = "black")

  patch <- ggb_obj$render()
  expect_s3_class(patch, "ggbrain_patchwork")

  panels <- ggb_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  layer_list <- panels[[1]]$gg$layers

  ann_layers <- vapply(layer_list, function(l) inherits(l$geom, "GeomText"), logical(1))
  expect_true(any(ann_layers))
})
