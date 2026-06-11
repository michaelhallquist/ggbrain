test_that("ggbrain accepts slices in the constructor", {
  arr <- array(1, dim = c(3, 3, 3))
  img <- ggbrain_images$new(images = list(test = RNifti::asNifti(arr)))

  expect_no_error(
    obj <- ggbrain(images = img, slices = c("k=1", "k=3"))
  )
  expect_identical(obj$ggb_images$slices, c("k=1", "k=3"))

  rendered <- obj + geom_brain("test")
  expect_s3_class(rendered$render(), "ggbrain_patchwork")
  expect_identical(rendered$ggb_plot$slices$coord_input, c("k=1", "k=3"))
})

test_that("images applies filters to cloned ggbrain_images objects", {
  arr <- array(0:7, dim = c(2, 2, 2))
  img <- ggbrain_images$new(images = list(test = RNifti::asNifti(arr)))

  filtered <- images(img, filter = "value > 3")
  filtered_values <- filtered$ggb_images$get_images("test")

  expect_equal(sort(unique(c(filtered_values))), c(0, 4:7))
  expect_equal(sum(filtered_values != 0), 4L)
  expect_equal(c(img$get_images("test")), 0:7)
})

test_that("images adds labels to cloned ggbrain_images objects", {
  arr <- array(rep(1:2, 4), dim = c(2, 2, 2))
  img <- ggbrain_images$new(images = list(test = RNifti::asNifti(arr)))
  labels <- data.frame(value = 1:2, label = c("one", "two"))

  labeled_list <- images(img, labels = list(test = labels))
  labeled_df <- images(img, labels = labels)

  list_labels <- labeled_list$ggb_images$get_labels()$test
  df_labels <- labeled_df$ggb_images$get_labels()$test
  expect_equal(list_labels$value, labels$value)
  expect_equal(list_labels$label, labels$label)
  expect_equal(df_labels$value, labels$value)
  expect_equal(df_labels$label, labels$label)
  expect_identical(attr(list_labels, "label_columns"), "label")
  expect_identical(attr(df_labels, "label_columns"), "label")
  expect_length(img$get_labels(), 0L)
})
