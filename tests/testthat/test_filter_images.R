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
