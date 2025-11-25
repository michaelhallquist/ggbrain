test_that("refine_image runs once per layer when unifying categorical levels", {
  refine_calls <- 0L

  library(ggplot2)

  # test double-dispatch of refine_image via data assignment
  TestLayer <- R6::R6Class(
    "TestLayer",
    inherit = ggbrain_layer_brain,
    private = list(
      refine_image = function() {
        refine_calls <<- refine_calls + 1L
        super$refine_image()
      }
    )
  )

  df1 <- data.frame(
    dim1 = c(1, 1, 2, 2),
    dim2 = c(1, 2, 1, 2),
    value = c(1, 1, 1, 1),
    region = c("A", "A", "B", "B")
  )
  attr(df1, "label_columns") <- "region"

  df2 <- data.frame(
    dim1 = c(1, 1, 2, 2),
    dim2 = c(1, 2, 1, 2),
    value = c(1, 1, 1, 1),
    region = c("B", "B", "B", "B")
  )
  attr(df2, "label_columns") <- "region"

  slice_df <- tibble::tibble(
    coord_input = list("z=1", "z=2"),
    coord_label = list("z=1", "z=2"),
    plane = list("z", "z"),
    slice_index = 1:2,
    slice_number = list(1, 2),
    slice_data = list(list(lay = df1), list(lay = df2)),
    slice_labels = list(list(), list()),
    slice_matrix = list(list(), list())
  )

  slices <- ggbrain_slices$new(slice_df)
  gp <- ggbrain_plot$new(slice_data = slices)

  layer <- TestLayer$new(
    name = "lay",
    definition = "lay",
    mapping = ggplot2::aes(fill = region),
    unify_scales = TRUE
  )
  layer$source <- "lay"

  gp$layers <- list(layer)
  gp$panel_settings <- replicate(length(slices$slice_index), list(), simplify = FALSE)

  gp$generate_plot()

  # one refine per slice/layer assignment, not per mutate of factor levels
  expect_equal(refine_calls, length(slices$slice_index))

  panels <- gp$.__enclos_env__$private$pvt_ggbrain_panels
  lay1 <- panels[[1]]$get_layers()[[1]]$data
  lay2 <- panels[[2]]$get_layers()[[1]]$data

  expect_equal(levels(lay1$region), c("A", "B"))
  expect_equal(levels(lay2$region), c("A", "B"))
})

test_that("fill_holes uses mode for labeled data even with fixed outline", {
  mat <- matrix(1, nrow = 5, ncol = 5)
  mat[4:5, ] <- 2
  mat[3, 3] <- NA

  df <- ggbrain:::mat2df(mat, na_zeros = FALSE)
  df$label <- ifelse(df$value == 1, "A", ifelse(df$value == 2, "B", NA))
  attr(df, "label_columns") <- "label"

  layer <- ggbrain_layer_outline$new(
    name = "lay",
    definition = "lay",
    outline = "black",
    fill_holes = 5
  )
  layer$data <- df

  filled_val <- subset(layer$data, dim1 == 2 & dim2 == 2)$value
  expect_true(filled_val %in% c(1, 2))
  expect_true(all(layer$data$value %% 1 == 0, na.rm = TRUE))
})
