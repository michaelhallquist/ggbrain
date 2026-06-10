test_that("nearest-neighbor imputation excludes invalid candidates", {
  mat <- matrix(0, nrow = 3, ncol = 3)
  mat[1, 1] <- 8
  mat[2, 2] <- NA_real_

  neighbors <- nearest_pts(
    x = 1L,
    y = 1L,
    in_mat = mat,
    neighbors = 4L,
    radius = 1L,
    ignore_zeros = TRUE
  )

  expect_equal(as.numeric(neighbors), 8)

  imputed <- nn_impute(
    mat,
    neighbors = 4L,
    radius = 1L,
    aggfun = "mean",
    ignore_zeros = TRUE
  )
  expect_equal(imputed[2, 2], 8)
})

test_that("nearest-neighbor imputation leaves values missing without valid neighbors", {
  mat <- matrix(0, nrow = 3, ncol = 3)
  mat[2, 2] <- NA_real_

  neighbors <- nearest_pts(
    x = 1L,
    y = 1L,
    in_mat = mat,
    neighbors = 4L,
    radius = 1L,
    ignore_zeros = TRUE
  )

  expect_length(neighbors, 0L)

  imputed <- nn_impute(
    mat,
    neighbors = 4L,
    radius = 1L,
    aggfun = "mean",
    ignore_zeros = TRUE
  )
  expect_true(is.na(imputed[2, 2]))
})
