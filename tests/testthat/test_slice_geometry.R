test_that("orientation labels follow increasing voxel indices on reversed axes", {
  volume <- RNifti::asNifti(array(1, c(4, 5, 6)))
  RNifti::sform(volume) <- structure(diag(c(-1, -1, -1, 1)), code = 2L)
  p <- ggbrain(images = ggbrain_images$new(list(underlay = volume))) +
    slices(c(" k=3", "j=3")) + geom_brain("underlay") + annotate_orientation()
  figure <- render(p)

  labels <- lapply(1:2, function(i) {
    data <- ggplot2::ggplot_build(figure[[i]])$data
    do.call(rbind, Filter(function(layer) "label" %in% names(layer), data))
  })
  expect_identical(labels[[1]]$label, c("R", "L", "A", "P"))
  expect_identical(labels[[2]]$label, c("R", "L", "S", "I"))
  expect_lt(labels[[1]]$x[1], labels[[1]]$x[2])
  expect_lt(labels[[1]]$y[3], labels[[1]]$y[4])
})

test_that("trimming sparse slices preserves voxel distances and empty slices", {
  values <- array(0, c(8, 7, 4))
  values[2, 2, 2] <- 4
  values[7, 6, 2] <- 9
  image <- ggbrain_images$new(list(map = RNifti::asNifti(values)))
  sl <- image$get_slices(c("k=2", "i=7", "k=3"), make_square = FALSE)

  axial <- sl$slice_matrix[[1]]$map
  expect_identical(dim(axial), c(6L, 5L))
  expect_equal(axial[cbind(c(1, 6), c(1, 5))], c(4, 9))
  expect_equal(sum(is.na(axial)), 28)
  expect_identical(sl$slice_grid[[1]]$dim1_vox, 2:7)
  expect_equal(sl$slice_matrix[[2]]$map, matrix(9))
  expect_identical(dim(sl$slice_matrix[[3]]$map), c(8L, 7L))
  expect_true(all(is.na(sl$slice_matrix[[3]]$map)))
  tiny <- image$get_slices("i=7", target_resolution = 10)
  expect_equal(tiny$slice_matrix[[1]]$map, matrix(9))
})

test_that("resampling uses in-plane spacing and retains fractional world coordinates", {
  values <- array(rep(1:7, 30), c(7, 6, 5))
  volume <- RNifti::asNifti(values)
  RNifti::pixdim(volume) <- c(1, 2, 3)
  image <- ggbrain_images$new(list(map = volume))
  sl <- image$get_slices(c("i=2", "j=2", "k=2"), make_square = FALSE,
                         target_resolution = 1, resample_interpolation = 3L)
  expect_identical(lapply(sl$slice_matrix, function(slice) dim(slice$map)),
                   list(c(12L, 15L), c(7L, 15L), c(7L, 12L)))
  axial <- sl$slice_grid[[3L]]
  expect_equal(axial$dim2_world, seq(0, 10, length.out = 12))
  expect_equal(sl$slice_matrix[[3L]]$map[, 6], 1:7)

  p <- ggbrain(images = image, slices = "k=2") + geom_brain("map")
  expect_identical(dim(p$get_slice_data("map", as_matrix = TRUE)[[1]]), c(7L, 6L))
  resized <- p + target_resolution(1)
  expect_identical(dim(resized$get_slice_data("map", as_matrix = TRUE)[[1]]), c(7L, 12L))
  expect_identical(dim(p$get_slice_data("map", as_matrix = TRUE)[[1]]), c(7L, 6L))
})

test_that("permuted axes are identified before reporting anatomical coordinates", {
  volume <- RNifti::asNifti(array(1, c(4, 5, 6)))
  affine <- diag(4)[c(2, 1, 3, 4), ]
  RNifti::sform(volume) <- structure(affine, code = 2L)
  image <- ggbrain_images$new(list(map = volume))
  expect_warning(sl <- image$get_slices("i=2"), "permuted or oblique")
  expect_identical(sl$slice_number, 2)
})
