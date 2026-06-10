test_that("semicolon conjunctions use first-match precedence and preserve labels", {
  dat <- data.frame(
    dim1 = 1:4,
    dim2 = rep(1L, 4),
    a.value = c(1, 1, 0, 0),
    b.value = c(1, 0, 1, 0)
  )
  attr(dat, "image") <- c("a", "b")

  result <- ggbrain:::contrast_parser(
    "both = a == 1 & b == 1; a_only = a == 1; b_only = b == 1",
    dat
  )

  expect_type(result$value, "integer")
  expect_identical(result$value, c(1L, 2L, 3L, NA_integer_))
  expect_identical(as.character(result$label), c("both", "a_only", "b_only", NA_character_))
  expect_s3_class(result$label, "factor")
  expect_identical(levels(result$label), c("both", "a_only", "b_only"))
  expect_identical(attr(result, "label_columns"), "label")
  expect_identical(attr(result, "categorical_fill_column"), "label")
  expect_identical(attr(result, "categorical_fill_levels"), c("both", "a_only", "b_only"))

  case_result <- ggbrain:::contrast_parser(
    "case_when(a == 1 & b == 1 ~ 'both', a == 1 ~ 'a_only', b == 1 ~ 'b_only')",
    dat
  )
  expect_identical(result$value, case_result$value)
  expect_identical(result$label, case_result$label)
})

test_that("case_when conjunctions use first-match precedence and quoted labels", {
  dat <- data.frame(
    dim1 = 1:4,
    dim2 = rep(1L, 4),
    a.value = c(1, 1, 0, 0),
    b.value = c(1, 0, 1, 0)
  )
  attr(dat, "image") <- c("a", "b")

  result <- ggbrain:::contrast_parser(
    paste(
      "dplyr::case_when(",
      "a == 1 & b == 1 ~ 'Both maps',",
      "a == 1 ~ 'A only',",
      "b == 1 ~ 'B only'",
      ")"
    ),
    dat
  )

  expect_identical(result$value, c(1L, 2L, 3L, NA_integer_))
  expect_identical(as.character(result$label), c("Both maps", "A only", "B only", NA_character_))
  expect_identical(levels(result$label), c("Both maps", "A only", "B only"))

  with_fallback <- ggbrain:::contrast_parser(
    "case_when(a == 1 ~ 'A present', TRUE ~ 'Other')",
    dat
  )
  expect_identical(as.character(with_fallback$label), c("A present", "A present", "Other", "Other"))
})

test_that("case_when conjunction validation is informative", {
  dat <- data.frame(dim1 = 1:2, dim2 = 1L, a.value = c(1, 0))
  attr(dat, "image") <- "a"

  expect_error(
    ggbrain:::contrast_parser("case_when(a == 1 ~ unquoted)", dat),
    "quoted character string"
  )
  expect_error(
    ggbrain:::contrast_parser("case_when(a == 1 ~ 'same', a == 0 ~ 'same')", dat),
    "labels must be unique"
  )
  expect_error(
    ggbrain:::contrast_parser("case_when(a ~ 'numeric condition')", dat),
    "must evaluate to TRUE or FALSE"
  )
  expect_error(
    ggbrain:::contrast_parser("same = a == 1; same = a == 0", dat),
    "labels must be unique"
  )
  expect_error(
    ggbrain:::contrast_parser("numeric_condition = a; other = a == 0", dat),
    "must evaluate to TRUE or FALSE"
  )
})

test_that("conjunction layers use categorical fill scales and user labels", {
  a <- array(c(
    1, 1, 0,
    0, 1, 0,
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0, 0, 0
  ), dim = c(3, 3, 2))
  b <- array(c(
    0, 1, 1,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    1, 1, 0,
    0, 0, 0
  ), dim = c(3, 3, 2))

  tf_a <- tempfile(fileext = ".nii.gz")
  tf_b <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(tf_a, tf_b)), add = TRUE)
  RNifti::writeNifti(a, tf_a)
  RNifti::writeNifti(b, tf_b)

  gg_obj <- ggbrain() +
    images(c(a = tf_a, b = tf_b)) +
    slices(c("k=1", "k=2")) +
    define("conjunction := z_category = a == 1 & b == 0; a_category = b == 1") +
    geom_brain(
      definition = "conjunction",
      fill_scale = ggplot2::scale_fill_brewer(palette = "Set2")
    )

  patch <- gg_obj$render()
  expect_silent(patchwork::patchworkGrob(patch))

  panels <- gg_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  conjunction_layers <- lapply(panels, function(panel) {
    Filter(
      function(layer) identical(layer$source, "conjunction"),
      panel$.__enclos_env__$private$pvt_layer_objs
    )[[1L]]
  })

  expect_true(all(vapply(conjunction_layers, function(layer) {
    isTRUE(layer$categorical_fill)
  }, logical(1))))
  expect_true(all(vapply(conjunction_layers, function(layer) {
    identical(layer$fill_column, "label")
  }, logical(1))))
  expect_true(all(vapply(conjunction_layers, function(layer) {
    is.numeric(layer$data$value) && is.factor(layer$data$label)
  }, logical(1))))
  expect_true(all(vapply(conjunction_layers, function(layer) {
    identical(levels(layer$data$label), c("z_category", "a_category"))
  }, logical(1))))

  default_obj <- ggbrain() +
    images(c(a = tf_a, b = tf_b)) +
    slices("k=1") +
    geom_brain("default_conjunction := z_category = a == 1 & b == 0; a_category = b == 1")

  default_patch <- default_obj$render()
  expect_silent(patchwork::patchworkGrob(default_patch))
  default_layer <- default_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels[[1]]$
    get_layers()[[1L]]

  expect_true(default_layer$categorical_fill)
  expect_s3_class(default_layer$fill_scale, "ScaleDiscrete")
})

test_that("case_when conjunctions render with discrete palettes", {
  a <- array(c(
    1, 1, 0,
    0, 1, 0,
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0, 0, 0
  ), dim = c(3, 3, 2))
  b <- array(c(
    1, 0, 1,
    0, 0, 0,
    0, 0, 0,
    0, 0, 0,
    1, 1, 0,
    0, 0, 0
  ), dim = c(3, 3, 2))

  tf_a <- tempfile(fileext = ".nii.gz")
  tf_b <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(tf_a, tf_b)), add = TRUE)
  RNifti::writeNifti(a, tf_a)
  RNifti::writeNifti(b, tf_b)

  gg_obj <- ggbrain() +
    images(c(a = tf_a, b = tf_b)) +
    slices(c("k=1", "k=2")) +
    define(conjunction := case_when(
      a == 1 & b == 1 ~ "Both maps",
      a == 1 ~ "A only",
      b == 1 ~ "B only"
    )) +
    geom_brain(
      "conjunction",
      fill_scale = ggplot2::scale_fill_brewer(palette = "Set2")
    )

  patch <- gg_obj$render()
  expect_silent(patchwork::patchworkGrob(patch))

  layer <- gg_obj$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels[[1]]$
    get_layers()[[1L]]
  expect_true(layer$categorical_fill)
  expect_identical(layer$fill_column, "label")
  expect_identical(levels(layer$data$label), c("Both maps", "A only", "B only"))
})
