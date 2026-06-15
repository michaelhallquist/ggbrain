make_cluster_contrast_images <- function() {
  dims <- c(9, 9, 9)
  a <- array(0, dim = dims)
  b <- array(0, dim = dims)
  template <- array(0, dim = dims)
  a[3:6, 5, 5] <- 2
  b[3:6, 5, 5] <- c(4, 3, 1, 0)
  template[3, 4:6, 4:6] <- 1

  ggbrain_images$new(images = list(
    a = RNifti::asNifti(a),
    b = RNifti::asNifti(b),
    template = RNifti::asNifti(template)
  ))
}

make_wide_contrast_data <- function(a, b) {
  data <- data.frame(
    dim1 = seq_along(a),
    dim2 = 1L,
    a.value = c(a),
    b.value = c(b)
  )
  attr(data, "image") <- c("a", "b")
  data
}

test_that("3D arithmetic contrast values match the slice contrast parser", {
  set.seed(20260613)
  dims <- c(4, 3, 2)
  a <- array(sample(-4:4, prod(dims), replace = TRUE), dim = dims)
  b <- array(sample(-3:3, prod(dims), replace = TRUE), dim = dims)
  images <- ggbrain_images$new(list(
    a = RNifti::asNifti(a),
    b = RNifti::asNifti(b)
  ))
  wide <- make_wide_contrast_data(a, b)

  definitions <- c(
    "a - b",
    "(a + b) / 2",
    "2 * a - b / 3",
    "-(a - b)",
    "abs(a - b)",
    "(a > b) & (b != 0)",
    "a[a > 0] - b",
    "a[a > 0] - b[b < 0]",
    "a[b >= 0] + b"
  )

  for (definition in definitions) {
    array_result <- evaluate_definition_3d(images, definition)$values
    slice_result <- contrast_parser(definition, wide)$value
    slice_result[is.na(slice_result)] <- 0

    expect_equal(
      c(array_result),
      slice_result,
      tolerance = 1e-12,
      info = definition
    )
  }
})

test_that("3D contrast evaluator supports whole-expression masks and value alias", {
  images <- make_cluster_contrast_images()
  a <- as.array(images$get_images("a"))
  b <- as.array(images$get_images("b"))
  difference <- a - b

  whole <- evaluate_definition_3d(images, "(a - b)[abs(a - b) > 1]")
  expected_whole <- difference
  expected_whole[abs(difference) <= 1] <- 0
  expect_equal(whole$values, expected_whole)
  expect_identical(whole$mask, abs(expected_whole) > images$zero_tol)

  legacy <- evaluate_definition_3d(images, "a[value > 1]")
  expected_legacy <- a
  expected_legacy[a <= 1] <- 0
  expect_equal(legacy$values, expected_legacy)

  logical_result <- evaluate_definition_3d(images, "abs(a - b) > 1")
  expect_equal(logical_result$values, array(as.numeric(abs(difference) > 1), dim = dim(a)))
  expect_identical(logical_result$mask, abs(difference) > 1)
})

test_that("3D contrast evaluator resolves named and recursive contrasts", {
  images <- make_cluster_contrast_images()
  contrasts <- list(
    difference = "a - b",
    scaled = "difference / 2",
    positive = "scaled[scaled > 0]"
  )

  result <- evaluate_definition_3d(images, "positive", contrasts)
  expected <- (as.array(images$get_images("a")) - as.array(images$get_images("b"))) / 2
  expected[expected <= 0] <- 0

  expect_equal(result$values, expected)
  expect_identical(result$reference_image, "a")
  expect_identical(result$mask, expected > images$zero_tol)

  inline_named <- evaluate_definition_3d(images, "inline := a - b")
  expect_equal(inline_named$values, as.array(images$get_images("a")) - as.array(images$get_images("b")))
})

test_that("3D contrast evaluator reports invalid definitions clearly", {
  images <- make_cluster_contrast_images()

  expect_error(
    evaluate_definition_3d(images, "a - missing"),
    "not found"
  )
  expect_error(
    evaluate_definition_3d(images, "first", list(first = "second + a", second = "first + b")),
    "Circular cluster contrast dependency.*first -> second -> first"
  )
  expect_error(
    evaluate_definition_3d(images, "case_when(a > 0 ~ 'positive')"),
    "Categorical case_when"
  )
  expect_error(
    evaluate_definition_3d(images, "positive = a > 0; negative = a < 0"),
    "one numeric or logical expression|Could not parse"
  )
  expect_error(
    evaluate_definition_3d(images, "mean(a)"),
    "returned a scalar"
  )
  expect_error(
    evaluate_definition_3d(images, "as.vector(a)"),
    "returned dimensions"
  )
  expect_error(
    evaluate_definition_3d(images, "a[a > 0, ]"),
    "exactly one condition"
  )
  expect_error(
    evaluate_definition_3d(images, "named", contrasts = list("a - b")),
    "must be named"
  )
  expect_error(
    evaluate_definition_3d(images, "named", contrasts = list(named = "a", named = "b")),
    "must be unique"
  )
})

test_that("3D contrast evaluator excludes missing and non-finite values", {
  a <- array(c(NA, Inf, -Inf, 0, 1, -2, 3, 0), dim = c(2, 2, 2))
  images <- ggbrain_images$new(list(a = RNifti::asNifti(a)))

  result <- evaluate_definition_3d(images, "a")

  expect_equal(c(result$values), c(0, 0, 0, 0, 1, -2, 3, 0))
  expect_identical(c(result$mask), c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE))
})

test_that("cluster sidedness is based on calculated contrast values", {
  images <- make_cluster_contrast_images()
  result <- compute_cluster_slices(
    images = images,
    definition = "a - b",
    nclusters = 2,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    sided = "bisided",
    outline = TRUE
  )

  expect_equal(nrow(result$cluster_data), 2L)
  expect_equal(sort(result$cluster_data$size), c(2L, 2L))
  expect_setequal(result$cluster_data$side, c("positive", "negative"))
  expect_identical(result$reference_image, "a")

  labeled_line <- result$labeled_volume[3:6, 5, 5]
  expect_length(unique(labeled_line[1:2]), 1L)
  expect_length(unique(labeled_line[3:4]), 1L)
  expect_false(labeled_line[1] == labeled_line[3])

  combined <- compute_cluster_slices(
    images = images,
    definition = "a - b",
    nclusters = 1,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    sided = "two_sided",
    outline = TRUE
  )
  expect_equal(nrow(combined$cluster_data), 1L)
  expect_identical(combined$cluster_data$size, 4L)
  expect_identical(combined$cluster_data$side, "two_sided")
})

test_that("direct, deferred, and layer-based contrast clustering agree", {
  images <- make_cluster_contrast_images()
  contrasts <- list(difference = "a - b")

  direct <- compute_cluster_slices(
    images, "difference", 2, 1, "sagittal", 1,
    sided = "bisided", outline = TRUE, contrasts = contrasts
  )

  deferred_spec <- cluster_slices(
    definition = "difference",
    nclusters = 2,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    outline = TRUE
  )
  deferred <- resolve_cluster_slices(
    deferred_spec,
    images,
    contrasts = contrasts
  )

  layer <- ggbrain_layer_brain$new(
    name = "difference_layer",
    definition = "a - b"
  )
  layer_spec <- cluster_slices(
    layer = "difference_layer",
    nclusters = 2,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    outline = TRUE
  )
  layer_result <- resolve_cluster_slices(layer_spec, images, layers = list(layer))

  expect_equal(deferred$cluster_data, direct$cluster_data)
  expect_identical(deferred$labeled_volume, direct$labeled_volume)
  expect_equal(layer_result$cluster_data, direct$cluster_data)
  expect_identical(layer_result$labeled_volume, direct$labeled_volume)
})

test_that("cluster_slices does not expose a local contrast namespace", {
  images <- make_cluster_contrast_images()

  expect_false("contrasts" %in% names(formals(cluster_slices)))
  expect_error(
    cluster_slices(
      images = images,
      definition = "difference",
      contrasts = list(difference = "a - b")
    ),
    "unused argument"
  )
})

test_that("defined contrasts reach deferred and clusterized render workflows", {
  images_obj <- make_cluster_contrast_images()

  deferred_plot <- ggbrain() +
    images(images_obj) +
    define(difference := a - b) +
    slices(cluster_slices(
      definition = "difference",
      nclusters = 2,
      min_clust_size = 1,
      plane = "sagittal",
      nn = 1
    )) +
    geom_brain("a", show_legend = FALSE)

  resolved <- deferred_plot$.__enclos_env__$private$resolve_cluster_specs(
    deferred_plot$ggb_images$clone(deep = TRUE)
  )
  deferred_data <- resolved$cluster_data[[1L]]
  expect_equal(nrow(deferred_data), 2L)
  expect_setequal(deferred_data$side, c("positive", "negative"))

  clusterized_plot <- ggbrain() +
    images(images_obj) +
    define(difference := a - b) +
    slices("i = 3") +
    geom_brain("a", show_legend = FALSE) +
    geom_brain_clusterized(
      "difference",
      nclusters = 2,
      min_clust_size = 1,
      nn = 1,
      show_legend = FALSE
    )

  expect_silent(clusterized_plot$render())
  clusterized_data <- clusterized_plot$get_cluster_data()
  expect_equal(nrow(clusterized_data), 2L)
  expect_setequal(clusterized_data$side, c("positive", "negative"))
  expect_true(any(vapply(
    clusterized_plot$ggb_plot$layers,
    function(layer) grepl("^clusterized", layer$name),
    logical(1L)
  )))
})

test_that("immediate cluster_slices accepts inline contrasts", {
  images <- make_cluster_contrast_images()

  result <- cluster_slices(
    images = images,
    definition = "(a - b)[a - b > 0]",
    nclusters = 1,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    outline = TRUE
  )

  expect_equal(nrow(result$cluster_data), 1L)
  expect_identical(result$cluster_data$side, "positive")
  expect_identical(result$cluster_data$size, 2L)
})
