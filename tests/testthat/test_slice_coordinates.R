coordinate_test_plot <- function() {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  ggbrain(bg_color = "white") +
    images(c(underlay = underlay)) +
    slices(c("z=20", "x=-30"), title = "Slice", xlab = "Position") +
    geom_brain("underlay", show_legend = TRUE)
}

coordinate_test_panels <- function(x) {
  x$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
}

test_that("slice coordinate captions reserve rows outside the image panels", {
  obj <- coordinate_test_plot() +
    annotate_coordinates(placement = "caption", colour = "red", size = 4, fontface = "bold") +
    annotate_orientation(show = "L")
  patch <- obj$render(guides = "collect")
  panels <- coordinate_test_panels(obj)

  expect_equal(vapply(panels, function(p) p$gg$labels$caption, character(1)), obj$ggb_plot$slices$coord_label)
  expect_identical(panels[[1]]$gg$labels$title, "Slice")
  expect_identical(panels[[1]]$gg$labels$x, "Position")
  caption_style <- panels[[1]]$gg$theme$plot.caption
  expect_identical(caption_style$colour, "red")
  expect_equal(caption_style$size, 4 * ggplot2::.pt)
  expect_identical(caption_style$face, "bold")
  expect_true(any(vapply(ggplot2::ggplot_build(panels[[1]]$gg)$data, function(d) any(d$label == "L"), logical(1))))

  layout <- patchwork::patchworkGrob(patch)$layout
  image_rows <- layout[grepl("^panel.*-[12]$", layout$name), ]
  caption_rows <- layout[grepl("^caption-[12]$", layout$name), ]
  expect_equal(nrow(caption_rows), 2L)
  expect_equal(caption_rows$l, image_rows$l)
  expect_true(all(caption_rows$t > image_rows$b))
})

test_that("panel edge coordinate labels get room while explicit positions still work", {
  obj <- coordinate_test_plot() + annotate_coordinates() +
    annotate_coordinates(x = "left", y = "top", vjust = 0.5, slice_index = 1) +
    annotate_coordinates(x = 25, y = "q50", hjust = 0.5, slice_index = 1)
  obj$render()
  panel <- coordinate_test_panels(obj)[[1]]$gg
  built <- ggplot2::ggplot_build(panel)
  rasters <- built$data[vapply(built$data, function(d) "ymin" %in% names(d), logical(1))]
  raster <- rasters[[1]]
  labels <- built$data[vapply(built$data, function(d) "label" %in% names(d), logical(1))]
  bottom <- labels[[1]]
  top <- labels[[2]]
  explicit <- labels[[3]]
  y_limits <- built$layout$panel_params[[1]]$y.range

  expect_lt(bottom$y, min(raster$ymin))
  expect_gt(bottom$y, y_limits[1])
  expect_lt(bottom$x, max(raster$x))
  expect_equal(c(bottom$hjust, bottom$vjust), c(1, 0))
  expect_gt(top$y, max(raster$ymax))
  expect_lt(top$y, y_limits[2])
  expect_gt(top$x, min(raster$x))
  expect_equal(c(top$hjust, top$vjust), c(0, 0.5))
  expect_equal(explicit$x, 25)
  expect_equal(explicit$y, unname(stats::quantile(raster$y, 0.5)))
  expect_equal(explicit$hjust, 0.5)
})

test_that("middle stays at the grid midpoint when a sparse overlay skews the quantiles", {
  obj <- coordinate_test_plot() +
    annotate_coordinates(x = "middle", y = "middle") +
    annotate_coordinates(x = "q50", y = "q50")
  obj$render()
  panel <- coordinate_test_panels(obj)[[1]]
  grid <- panel$get_data()[[1]][c("dim1", "dim2", "value")]
  midpoint <- c(mean(range(grid$dim1)), mean(range(grid$dim2)))
  sparse <- grid[grid$dim1 > midpoint[1] & grid$dim2 > midpoint[2], ]
  panel$add_layer(ggbrain_layer_brain$new(
    name = "sparse", definition = "sparse", data = sparse,
    mapping = NULL, fill = "cyan", show_legend = FALSE
  ))

  built <- ggplot2::ggplot_build(panel$gg)
  labels <- Filter(function(d) "label" %in% names(d), built$data)
  pooled <- rbind(grid, sparse)
  medians <- c(stats::median(pooled$dim1), stats::median(pooled$dim2))
  expect_true(all(medians > midpoint))
  expect_equal(c(labels[[1]]$x, labels[[1]]$y), midpoint)
  expect_equal(c(labels[[2]]$x, labels[[2]]$y), medians)
})

test_that("q1 means the first percentile and q1.0 means the maximum", {
  path <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(path), add = TRUE)
  RNifti::writeNifti(array(1, dim = c(130, 120, 2)), path)
  obj <- ggbrain() + images(c(underlay = path)) + slices("k=1") +
    geom_brain("underlay", show_legend = FALSE) +
    annotate_coordinates(x = "q1", y = "q1.0") +
    annotate_slice(1, x = "q1.0", y = "q1", label = "custom")
  obj$render()

  built <- ggplot2::ggplot_build(coordinate_test_panels(obj)[[1]]$gg)
  raster <- built$data[[1]]
  labels <- Filter(function(d) "label" %in% names(d), built$data)
  expect_equal(labels[[1]]$x, stats::quantile(raster$x, 0.01, names = FALSE))
  expect_equal(labels[[1]]$y, max(raster$y))
  expect_equal(labels[[2]]$x, max(raster$x))
  expect_equal(labels[[2]]$y, stats::quantile(raster$y, 0.01, names = FALSE))
})
