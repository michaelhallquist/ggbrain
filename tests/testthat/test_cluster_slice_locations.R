test_that("peak slices use signed contrast magnitude within size-ranked clusters", {
  values <- array(0, c(8, 7, 5))
  values[2:4, 2, 2] <- c(4, 8, 8)
  values[4:7, 5, 4] <- c(-2, -3, -4, -7)
  values[8, 7, 1] <- 99
  reference <- array(1, dim(values))
  img <- ggbrain_images$new(list(
    observed = RNifti::asNifti(values + reference),
    reference = RNifti::asNifti(reference)
  ))
  definition <- "(observed - reference)[abs(observed - reference) > 1]"

  result <- cluster_slices(img, definition = definition, nclusters = 2, min_clust_size = 2,
                           plane = "sagittal", nn = 1, slice_at = "peak")
  expect_identical(result$coordinates, c("i = 7", "i = 3"))
  expect_identical(result$cluster_data$side, c("negative", "positive"))
  expect_identical(result$cluster_data$size, c(4L, 3L))
  expect_equal(result$cluster_data$com_i, c(5.5, 3))
  expect_identical(result$cluster_data$slice_value, c(-7, 8))
  expect_null(result$labeled_volume)
  expect_null(attr(result$cluster_data, "labeled_volume"))

  outlined <- cluster_slices(img, definition = definition, nclusters = 2, min_clust_size = 2,
                             plane = "sagittal", nn = 1, slice_at = "peak", outline = TRUE)
  selected_voxels <- as.matrix(outlined$cluster_data[, c("slice_i", "slice_j", "slice_k")])
  expect_identical(outlined$labeled_volume[selected_voxels], c(1L, 2L))
  expect_true(is.na(outlined$labeled_volume[8, 7, 1]))
})

test_that("nearest cluster voxels are selected by physical distance", {
  values <- array(0, c(5, 5, 5))
  values[2:4, 3, 2:4] <- 5
  values[3, 3, 3] <- 0
  volume <- RNifti::asNifti(values)
  RNifti::sform(volume) <- structure(diag(c(1, 1, 4, 1)), code = 2L)
  img <- ggbrain_images$new(list(map = volume))

  original <- cluster_slices(img, definition = "map", nclusters = 1, plane = "sagittal", nn = 1)
  expect_identical(original$coordinates, "x = 2")
  legacy_spec <- cluster_slices(definition = "map", nclusters = 1, plane = "sagittal", nn = 1)
  legacy_spec$slice_at <- NULL
  expect_identical(resolve_cluster_slices(legacy_spec, img), original)

  nearest <- cluster_slices(img, definition = "map", nclusters = 1, plane = "sagittal", nn = 1,
                            slice_at = "nearest")
  expect_identical(nearest$coordinates, "i = 2")
  expect_equal(nearest$cluster_data[, c("com_i", "com_j", "com_k")],
               data.frame(com_i = 3, com_j = 3, com_k = 3))
  expect_identical(nearest$cluster_data[, c("slice_i", "slice_j", "slice_k", "slice_value")],
                   data.frame(slice_i = 2L, slice_j = 3L, slice_k = 3L, slice_value = 5))
})

crosshair_segments <- function(panel) {
  data <- ggplot2::ggplot_build(panel)$data
  dplyr::bind_rows(Filter(function(layer) all(c("xend", "yend") %in% names(layer)), data))
}

test_that("peak slices and crosshairs use exact voxels even on a fine grid", {
  values <- array(0, c(7, 7, 4))
  values[2:3, 2:3, 1] <- 3
  values[2:3, 2:3, 2] <- 2
  values[5:6, 5:6, 1] <- -4
  values[5:6, 5:6, 2] <- -2
  volume <- RNifti::asNifti(values)
  affine <- diag(c(1, 1, 0.03, 1))
  affine[3, 4] <- 0.02
  RNifti::sform(volume) <- structure(affine, code = 2L)
  img <- ggbrain_images$new(list(map = volume))

  p <- ggbrain(images = img) +
    define(active := map[abs(map) > 1]) +
    slices(c("k = 2", "i = 2")) +
    slices(cluster_slices(layer = "Activation", nclusters = 2, slice_at = "peak", outline = TRUE)) +
    geom_brain("active", name = "Activation") +
    annotate_crosshairs(at = "cluster_peaks", color = "gold", linewidth = 0.8,
                        linetype = "solid", lineend = "round")

  expect_silent(figure <- render(p))
  expect_equal(p$ggb_plot$slices$slice_number, c(2, 2, 1))
  report <- p$get_cluster_data()
  expect_identical(report$slice_coord, rep("k = 1", 2))
  expect_identical(report$slice_value, c(3, -4))

  expect_equal(nrow(crosshair_segments(figure[[1]])), 0L)
  expect_equal(nrow(crosshair_segments(figure[[2]])), 2L)
  axial <- crosshair_segments(figure[[3]])
  expect_equal(nrow(axial), 4L)
  expect_equal(axial$x[axial$x == axial$xend], c(1, 4))
  expect_equal(axial$y[axial$y == axial$yend], c(1, 4))
  expect_true(all(axial$colour == "gold" & axial$linewidth == 0.8 & axial$linetype == "solid"))
  expect_equal(crosshair_segments(render(p)[[3]]), axial)
})

test_that("peak selection respects two-sided connectivity and empty selections", {
  values <- array(0, c(4, 5, 4))
  values[2, 2:3, 2] <- c(5, -6)
  img <- ggbrain_images$new(list(map = RNifti::asNifti(values)))
  result <- cluster_slices(img, definition = "map", nclusters = 1, plane = "coronal", nn = 1,
                           sided = "two_sided", slice_at = "peak")
  expect_identical(result$coordinates, "j = 3")
  expect_identical(result$cluster_data$slice_value, -6)
  expect_identical(result$cluster_data$size, 2L)

  expect_warning(
    empty <- cluster_slices(img, definition = "map[abs(map) > 10]", slice_at = "nearest"),
    "No clusters found"
  )
  expect_identical(empty$coordinates, character())
  expect_identical(empty$cluster_data$slice_value, numeric())
  expect_error(cluster_slices(definition = "map", slice_at = "unknown"), "should be one of")
})

test_that("peak crosshairs ignore nearest-voxel selections and coexist with world coordinates", {
  values <- array(0, c(5, 5, 5))
  values[2:3, 2:3, 2:3] <- 2
  img <- ggbrain_images$new(list(map = RNifti::asNifti(values)))
  p <- ggbrain(images = img) +
    slices(cluster_slices(definition = "map", nclusters = 1, slice_at = "nearest")) +
    geom_brain("map") +
    annotate_crosshairs(at = "cluster_peaks")

  expect_silent(figure <- render(p))
  expect_equal(nrow(crosshair_segments(figure[[1]])), 0L)

  with_xyz <- p + annotate_crosshairs(data.frame(x = 2, y = 2, z = NA))
  manual <- crosshair_segments(render(with_xyz)[[1]])
  expect_equal(nrow(manual), 2L)
  expect_equal(manual$x[manual$x == manual$xend], 2)
  expect_equal(manual$y[manual$y == manual$yend], 2)
  expect_error(annotate_crosshairs(data.frame(x = 0, y = 0, z = 0), at = "cluster_peaks"),
               "mutually exclusive")
})
