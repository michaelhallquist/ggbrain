test_that("define captures unquoted arithmetic expressions", {
  definition <- define(zdiff := img1 - img2)

  expect_named(definition$ggb_contrasts, "zdiff")
  expect_identical(definition$ggb_contrasts$zdiff, "img1 - img2")
})

test_that("define preserves existing character inputs", {
  from_string <- define("zdiff := img1 - img2")
  from_named <- define(c(zdiff = "img1 - img2"))

  expect_identical(from_string$ggb_contrasts, from_named$ggb_contrasts)
})

test_that("define requires a simple name for unquoted definitions", {
  expect_error(
    define("not a name" := img1 - img2),
    "simple name"
  )
})
