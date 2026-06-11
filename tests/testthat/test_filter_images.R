test_that("filter_images applies character filters without labels", {
  arr <- array(0:7, dim = c(2, 2, 2))
  obj <- ggbrain_images$new(
    images = list(test = RNifti::asNifti(arr)),
    filter = "value > 3"
  )

  vals <- obj$get_images("test")
  expect_true(all(vals[vals != 0] > 3))
  expect_equal(sum(vals != 0), 4)
})

test_that("filter_images retains only requested numeric values", {
  arr <- array(0:7, dim = c(2, 2, 2))
  obj <- ggbrain_images$new(
    images = list(test = RNifti::asNifti(arr)),
    filter = list(test = c(2, 7))
  )

  vals <- obj$get_images("test")
  expect_equal(sort(unique(c(vals))), c(0, 2, 7))
  expect_equal(sum(vals != 0), 2)
})

test_that("filter_images applies nested and vectorized character filters", {
  arr <- array(0:7, dim = c(2, 2, 2))

  nested <- ggbrain_images$new(
    images = list(test = RNifti::asNifti(arr)),
    filter = list(test = list("value > 1", "value < 6"))
  )
  vectorized <- ggbrain_images$new(
    images = list(test = RNifti::asNifti(arr)),
    filter = list(test = c("value > 1", "value < 6"))
  )

  expect_equal(sort(unique(c(nested$get_images("test")))), c(0, 2:5))
  expect_equal(sort(unique(c(vectorized$get_images("test")))), c(0, 2:5))
  expect_equal(sum(nested$get_images("test") != 0), 4L)
  expect_equal(sum(vectorized$get_images("test") != 0), 4L)
})

test_that("image mutations refresh cached nonzero support", {
  first <- array(0, dim = c(5, 2, 2))
  first[1, 1, 1] <- 1
  second <- array(0, dim = c(5, 2, 2))
  second[5, 1, 1] <- 2

  obj <- ggbrain_images$new(images = list(first = RNifti::asNifti(first)))
  expect_equal(obj$get_nz_indices()$i, c(1L, 1L))

  obj$add_array_as_image(second, "second")
  expect_equal(obj$get_nz_indices()$i, c(1L, 5L))

  obj$filter_images(list(first = 999))
  expect_equal(obj$get_nz_indices()$i, c(5L, 5L))
})

test_that("remove_images synchronizes names, labels, support, and geometry", {
  first <- array(0, dim = c(5, 2, 2))
  first[1, 1, 1] <- 1
  second <- array(0, dim = c(5, 2, 2))
  second[5, 1, 1] <- 2
  labels <- data.frame(value = 2, label = "second")

  obj <- ggbrain_images$new(
    images = list(
      first = RNifti::asNifti(first),
      second = RNifti::asNifti(second)
    ),
    labels = list(second = labels)
  )

  expect_message(obj$remove_images("second"), "Removing images")
  expect_identical(obj$get_image_names(), "first")
  expect_length(obj$get_labels(), 0L)
  expect_equal(obj$get_nz_indices()$i, c(1L, 1L))
  expect_error(obj$get_images("second"))

  expect_message(obj$remove_images("first"), "Removing images")
  expect_null(obj$get_image_names())
  expect_null(obj$dim())

  replacement <- array(1, dim = c(3, 3, 3))
  expect_silent(obj$add_array_as_image(
    replacement,
    "replacement",
    reference = RNifti::asNifti(replacement)
  ))
  expect_equal(obj$dim(), c(3L, 3L, 3L))
  expect_identical(obj$get_image_names(), "replacement")
})

test_that("replacing image data clears labels from the previous image", {
  original <- RNifti::asNifti(array(1, dim = c(2, 2, 2)))
  replacement <- RNifti::asNifti(array(2, dim = c(2, 2, 2)))
  labels <- data.frame(value = 1, label = "original")

  obj <- ggbrain_images$new(
    images = list(atlas = original),
    labels = list(atlas = labels)
  )

  obj$add_images(list(atlas = replacement))

  expect_length(obj$get_labels(), 0L)
  expect_equal(unique(c(obj$get_images("atlas"))), 2)
})

test_that("replacing an image with an array clears its previous labels", {
  original <- RNifti::asNifti(array(1, dim = c(2, 2, 2)))
  labels <- data.frame(value = 1, label = "original")
  obj <- ggbrain_images$new(
    images = list(atlas = original),
    labels = list(atlas = labels)
  )

  obj$add_array_as_image(array(3, dim = c(2, 2, 2)), "atlas")

  expect_length(obj$get_labels(), 0L)
  expect_equal(unique(c(obj$get_images("atlas"))), 3)
})

test_that("object addition replaces labels together with colliding image data", {
  original_labels <- data.frame(value = 1, label = "original")
  replacement_labels <- data.frame(value = 2, label = "replacement")
  target <- ggbrain_images$new(
    images = list(atlas = RNifti::asNifti(array(1, dim = c(2, 2, 2)))),
    labels = list(atlas = original_labels)
  )
  source <- ggbrain_images$new(
    images = list(atlas = RNifti::asNifti(array(2, dim = c(2, 2, 2)))),
    labels = list(atlas = replacement_labels)
  )

  target$add(source)

  expect_identical(target$get_labels()$atlas$label, "replacement")
  expect_equal(unique(c(target$get_images("atlas"))), 2)

  unlabeled_source <- ggbrain_images$new(
    images = list(atlas = RNifti::asNifti(array(4, dim = c(2, 2, 2))))
  )
  target$add(unlabeled_source)

  expect_length(target$get_labels(), 0L)
  expect_equal(unique(c(target$get_images("atlas"))), 4)
})
