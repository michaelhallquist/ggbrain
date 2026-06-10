test_that("as_tibble preserves numeric slice numbers", {
  layer_data <- data.frame(
    dim1 = 1L,
    dim2 = 1L,
    value = 1
  )

  slice_df <- data.frame(
    coord_input = c("i=3", "k=7"),
    coord_label = c("x = 3", "z = 7"),
    plane = c("sagittal", "axial"),
    slice_index = 1:2,
    slice_number = c(3L, 7L)
  )
  slice_df$slice_data <- list(
    list(image = layer_data),
    list(image = layer_data)
  )
  slice_df$slice_labels <- list(list(), list())
  slice_df$slice_matrix <- list(
    list(image = matrix(1)),
    list(image = matrix(1))
  )

  slices_obj <- ggbrain_slices$new(slice_df)
  result <- slices_obj$as_tibble()

  expect_identical(result$slice_number, c(3L, 7L))
  expect_type(result$slice_number, "integer")
})
