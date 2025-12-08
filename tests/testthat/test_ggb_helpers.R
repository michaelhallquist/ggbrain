test_that("compute_layer_sources parses inline contrasts and image sources", {
  arr <- array(1:8, dim = c(2, 2, 2))
  img_obj <- ggbrain_images$new(images = list(a = RNifti::asNifti(arr)))

  g <- ggb$new(images = img_obj)
  priv <- g$.__enclos_env__$private

  defs <- c("a", "c := a * 2")
  res <- priv$compute_layer_sources(defs, img_obj, cluster_layer_names = character(0))

  expect_identical(res$layer_sources, c("a", "c"))
  expect_named(res$inline_contrasts, "c")
  expect_equal(length(res$inline_contrasts), 1L)
})

test_that("assign_layer_sources updates layer source fields", {
  arr <- array(1:8, dim = c(2, 2, 2))
  img_obj <- ggbrain_images$new(images = list(a = RNifti::asNifti(arr)))
  g <- ggb$new(images = img_obj)

  l1 <- ggbrain_layer_brain$new(name = "a", definition = "a")
  l2 <- ggbrain_layer_brain$new(name = "c", definition = "c")
  g$add_layers(list(l1, l2))

  sources <- c("a", "contrast_c")
  g$.__enclos_env__$private$assign_layer_sources(sources)

  expect_identical(g$ggb_layers[[1]]$source, "a")
  expect_identical(g$ggb_layers[[2]]$source, "contrast_c")
})

test_that("resolve_cluster_specs no-ops when no cluster specs present", {
  arr <- array(1:8, dim = c(2, 2, 2))
  img_obj <- ggbrain_images$new(images = list(a = RNifti::asNifti(arr)))
  g <- ggb$new(images = img_obj)
  priv <- g$.__enclos_env__$private

  res <- priv$resolve_cluster_specs(img_obj$clone(deep = TRUE))

  expect_identical(res$img$get_image_names(), img_obj$get_image_names())
  expect_length(res$outline_imgs, 0L)
  expect_identical(g$ggb_slices, NULL)
})
