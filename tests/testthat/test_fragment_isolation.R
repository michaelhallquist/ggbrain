test_that("reused layer fragments are isolated between parent plots", {
  fragment <- geom_brain("underlay", alpha = 0.8)

  plot_a <- ggbrain() + fragment
  plot_b <- ggbrain() + fragment

  expect_false(identical(plot_a$ggb_layers[[1L]], plot_b$ggb_layers[[1L]]))
  expect_false(identical(plot_a$ggb_layers[[1L]], fragment$ggb_layers[[1L]]))

  plot_a$ggb_layers[[1L]]$alpha <- 0.2

  expect_equal(plot_a$ggb_layers[[1L]]$alpha, 0.2)
  expect_equal(plot_b$ggb_layers[[1L]]$alpha, 0.8)
  expect_equal(fragment$ggb_layers[[1L]]$alpha, 0.8)
})

test_that("reused region label fragments are isolated between parent plots", {
  fragment <- geom_region_text("atlas", min_px = 2L, color = "black")

  plot_a <- ggbrain() + fragment
  plot_b <- ggbrain() + fragment

  expect_false(identical(plot_a$ggb_region_labels[[1L]], plot_b$ggb_region_labels[[1L]]))
  expect_false(identical(plot_a$ggb_region_labels[[1L]], fragment$ggb_region_labels[[1L]]))

  plot_a$ggb_region_labels[[1L]]$min_px <- 5L
  plot_a$ggb_region_labels[[1L]]$addl_args$color <- "red"

  expect_identical(plot_b$ggb_region_labels[[1L]]$min_px, 2L)
  expect_identical(plot_b$ggb_region_labels[[1L]]$addl_args$color, "black")
  expect_identical(fragment$ggb_region_labels[[1L]]$min_px, 2L)
  expect_identical(fragment$ggb_region_labels[[1L]]$addl_args$color, "black")
})

test_that("rendering does not write slice data into region label specifications", {
  atlas_img <- system.file("extdata", "Schaefer_200_7networks_2009c_2mm.nii.gz", package = "ggbrain")
  atlas_labels <- read.csv(system.file("extdata", "Schaefer_200_7networks_labels.csv", package = "ggbrain"))
  atlas_labels$value <- atlas_labels$roi_num

  fragment <- geom_region_text("atlas", label_column = "network", min_px = 1L)
  plot_obj <- ggbrain() +
    images(c(atlas = atlas_img), labels = list(atlas = atlas_labels)) +
    slices(c("z=20", "z=40")) +
    geom_brain("atlas", show_legend = FALSE) +
    fragment

  expect_null(plot_obj$ggb_region_labels[[1L]]$data)
  expect_null(fragment$ggb_region_labels[[1L]]$data)

  plot_obj$render()

  expect_null(plot_obj$ggb_region_labels[[1L]]$data)
  expect_null(fragment$ggb_region_labels[[1L]]$data)
  expect_null(plot_obj$ggb_plot$region_labels[[1L]]$data)

  panels <- plot_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  panel_labels <- lapply(panels, function(panel) {
    panel$.__enclos_env__$private$pvt_region_labels[[1L]]
  })

  expect_length(panel_labels, 2L)
  expect_false(identical(panel_labels[[1L]], panel_labels[[2L]]))
  expect_true(all(vapply(panel_labels, function(label) !is.null(label$data), logical(1L))))
})
