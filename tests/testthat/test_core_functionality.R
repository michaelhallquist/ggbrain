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

test_that("unified scales default to bisided when any slice has mixed signs", {
  # synthetic 3-slice overlay with mixed signs only in some slices
  arr_underlay <- array(1, dim = c(5, 5, 3)) # avoid all-NA underlay after zero trimming
  arr_overlay <- array(0, dim = c(5, 5, 3))
  arr_overlay[, , 1] <- matrix(c(-2, 2, 0, 0, 0,
                                 0, -1, 0, 0, 1,
                                 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0,
                                 0, 0, 0, 0, 0), nrow = 5, byrow = TRUE)
  arr_overlay[, , 2] <- -1 # all negative
  arr_overlay[, , 3] <- 2  # all positive

  tf_under <- tempfile(fileext = ".nii.gz")
  tf_over <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(tf_under, tf_over)), add = TRUE)
  RNifti::writeNifti(arr_underlay, tf_under)
  RNifti::writeNifti(arr_overlay, tf_over)

  gg_obj <- ggbrain() +
    images(c(underlay = tf_under, overlay = tf_over)) +
    slices(c("k=1", "k=2", "k=3")) +
    geom_brain("underlay") +
    geom_brain("overlay", unify_scales = TRUE)

  gg_obj$render()

  panels <- gg_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  overlay_layers <- lapply(panels, function(pn) {
    ply <- pn$.__enclos_env__$private$pvt_layer_objs
    Filter(function(x) identical(x$source, "overlay"), ply)[[1L]]
  })

  expect_true(all(vapply(overlay_layers, function(l) isTRUE(l$bisided), logical(1))))
})

test_that("inline factor mapping materializes label column before unify scales", {
  arr <- array(rep(c(1:4), length.out = 27), dim = c(3, 3, 3))
  tf <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tf), add = TRUE)
  RNifti::writeNifti(arr, tf)

  gg_obj <- ggbrain() +
    images(c(test = tf)) +
    slices(c("k=1", "k=2")) +
    geom_brain(definition = "test", mapping = aes(fill = factor(label_col)), unify_scales = TRUE, show_legend = FALSE)

  gg_obj$render()

  panels <- gg_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  layer_data <- lapply(panels, function(pn) {
    ply <- pn$.__enclos_env__$private$pvt_layer_objs
    Filter(function(x) identical(x$source, "test"), ply)[[1L]]$data
  })

  # The synthetic factor mapping should materialize label_col (copied from value) as factor; value remains numeric
  expect_true(all(vapply(layer_data, function(df) is.factor(df$label_col), logical(1))))
  expect_true(all(vapply(layer_data, function(df) is.numeric(df$value), logical(1))))
})
