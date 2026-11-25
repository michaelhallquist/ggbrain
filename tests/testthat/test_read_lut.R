test_that("read_lut parses packaged FreeSurfer Yeo 7-network LUT", {
  lut_path <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")

  lut <- read_lut(lut_path)
  expect_s3_class(lut, "tbl_df")
  expect_named(lut, c("value", "label", "r", "g", "b", "color"))
  expect_equal(nrow(lut), 7L)
  expect_identical(lut$value, 1:7)
  expect_identical(lut$label, paste0("7Networks_", 1:7))
  expect_identical(lut$r[1L], 120L)
  expect_identical(lut$g[1L], 18L)
  expect_identical(lut$b[1L], 134L)
  expect_identical(lut$color[1L], "#781286")
  expect_identical(lut$color[7L], "#CD3E4E")

  # drop_zero = FALSE keeps the 0 NONE entry
  lut_all <- read_lut(lut_path, drop_zero = FALSE)
  expect_equal(nrow(lut_all), 8L)
  expect_identical(lut_all$value[1L], 0L)
  expect_identical(lut_all$label[1L], "NONE")
  expect_identical(lut_all$color[1L], "#000000")
})

test_that("read_lut handles FreeSurfer LUT comments, 5-column rows, and apostrophes", {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c(
    "# FreeSurfer ColorLUT example",
    "",
    "  0   Unknown             0   0   0   0",
    "  1   Left-Heschl's-Gyrus 255 128 0   0  # inline comment",
    "  2   Right-Cortex        10  20  30"
  ), tf)

  lut <- read_lut(tf)
  expect_equal(nrow(lut), 2L)
  expect_identical(lut$value, c(1L, 2L))
  expect_identical(lut$label, c("Left-Heschl's-Gyrus", "Right-Cortex"))
  expect_identical(lut$color, c("#FF8000", "#0A141E"))
})

test_that("read_lut auto-detects and parses ITK-SNAP label description files", {
  tf <- tempfile(fileext = ".label")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c(
    "################################################",
    "# ITK-SnAP Label Description File",
    "# IDX   -R-  -G-  -B-  -A-  VIS MSH  LABEL",
    "################################################",
    '    0     0    0    0        0  0  0    "Clear Label"',
    '    1   255    0    0        1  1  1    "Left Hippocampus"',
    '    2     0  128  255        1  1  1    "Right Hippocampus #2"'
  ), tf)

  lut <- read_lut(tf)
  expect_equal(nrow(lut), 2L)
  expect_identical(lut$value, c(1L, 2L))
  expect_identical(lut$label, c("Left Hippocampus", "Right Hippocampus #2"))
  expect_identical(lut$r, c(255L, 0L))
  expect_identical(lut$g, c(0L, 128L))
  expect_identical(lut$b, c(0L, 255L))
  expect_identical(lut$color, c("#FF0000", "#0080FF"))

  # Also works without the ITK-SnAP comment header via column-structure auto-detection
  tf_nohdr <- tempfile(fileext = ".txt")
  on.exit(unlink(tf_nohdr), add = TRUE)
  writeLines(c(
    '1 255 0 0 1 1 1 "Left Hippocampus"',
    '2 0 255 0 1 1 1 "Right Hippocampus"'
  ), tf_nohdr)
  lut_nohdr <- read_lut(tf_nohdr)
  expect_identical(lut_nohdr$label, c("Left Hippocampus", "Right Hippocampus"))
  expect_identical(lut_nohdr$color, c("#FF0000", "#00FF00"))
})

test_that("read_lut auto-detects and parses FSL/FSLeyes LUTs with 0-1 RGB and multi-word labels", {
  tf <- tempfile(fileext = ".lut")
  on.exit(unlink(tf), add = TRUE)
  writeLines(c(
    "# FSLeyes label lookup table",
    "0 0.0 0.0 0.0 Background",
    "1 1.0 0.0 0.0 Left Thalamus",
    "2 0.0 0.5 1.0 Right Heschl's Gyrus"
  ), tf)

  lut <- read_lut(tf)
  expect_equal(nrow(lut), 2L)
  expect_identical(lut$value, c(1L, 2L))
  expect_identical(lut$label, c("Left Thalamus", "Right Heschl's Gyrus"))
  expect_identical(lut$r, c(255L, 0L))
  expect_identical(lut$g, c(0L, 128L))
  expect_identical(lut$b, c(0L, 255L))
  expect_identical(lut$color, c("#FF0000", "#0080FF"))

  # FSL-style index R G B label with 0-255 integer RGB also works
  tf_255 <- tempfile(fileext = ".lut")
  on.exit(unlink(tf_255), add = TRUE)
  writeLines(c(
    "1 200 100 50 Frontal Pole",
    "2 10 20 30 Insular Cortex"
  ), tf_255)
  lut_255 <- read_lut(tf_255)
  expect_identical(lut_255$label, c("Frontal Pole", "Insular Cortex"))
  expect_identical(lut_255$color, c("#C86432", "#0A141E"))
})

test_that("read_lut validates malformed inputs clearly", {
  tf_empty <- tempfile(fileext = ".txt")
  tf_bad_rgb <- tempfile(fileext = ".txt")
  tf_bad_row <- tempfile(fileext = ".txt")
  on.exit(unlink(c(tf_empty, tf_bad_rgb, tf_bad_row)), add = TRUE)

  writeLines(c("# only comments", "   "), tf_empty)
  expect_error(read_lut(tf_empty), "No LUT entries")

  writeLines("1 RegionA 300 0 0 0", tf_bad_rgb)
  expect_error(read_lut(tf_bad_rgb), "within \\[0, 1\\] or \\[0, 255\\]")

  writeLines(c("1 RegionA 10 20 30", "2 RegionB 10"), tf_bad_row)
  expect_error(read_lut(tf_bad_row, format = "freesurfer"), "line 2")
})

test_that("scale_fill_lut constructs discrete fill scales from data.frames and file paths", {
  lut_path <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")
  yeo_lut <- read_lut(lut_path)

  sc_df <- scale_fill_lut(yeo_lut, name = "Yeo 7")
  expect_s3_class(sc_df, "ScaleDiscrete")
  expect_identical(sc_df$name, "Yeo 7")
  expect_identical(sc_df$na.value, "transparent")
  expect_identical(unname(sc_df$palette(7L)[1L]), "#781286")

  # Directly from file path
  sc_file <- scale_fill_lut(lut_path)
  expect_s3_class(sc_file, "ScaleDiscrete")
  expect_identical(sc_file$palette(7L), sc_df$palette(7L))

  # Mapping by integer value or custom column
  sc_val <- scale_fill_lut(yeo_lut, by = "value")
  expect_s3_class(sc_val, "ScaleDiscrete")

  yeo_lut$network <- c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic", "Cont", "Default")
  sc_net <- scale_fill_lut(yeo_lut, by = "network")
  expect_identical(sc_net$palette(7L)[["Vis"]], "#781286")

  # Duplicate key handling: identical colors deduplicate silently; conflicting colors warn
  dup_same <- rbind(yeo_lut[1:2, ], yeo_lut[1, ])
  expect_silent(scale_fill_lut(dup_same))

  dup_diff <- dup_same
  dup_diff$color[3L] <- "#000000"
  expect_warning(sc_dup <- scale_fill_lut(dup_diff), "conflicting colors")
  expect_identical(sc_dup$palette(2L)[["7Networks_1"]], "#781286")
})

test_that("read_lut and scale_fill_lut integrate end-to-end with ggbrain plots and outlines", {
  dims <- c(6, 6, 2)
  underlay <- array(50, dim = dims)
  atlas <- array(0, dim = dims)
  atlas[2:3, 2:3, ] <- 1
  atlas[4:5, 2:3, ] <- 2

  tf_lut <- tempfile(fileext = ".txt")
  on.exit(unlink(tf_lut), add = TRUE)
  writeLines(c(
    '0 0 0 0 0 0 0 "Background"',
    '1 255 0 0 1 1 1 "Left Striatum"',
    '2 0 0 255 1 1 1 "Right Striatum"'
  ), tf_lut)

  lut <- read_lut(tf_lut)

  p <- ggbrain() +
    images(list(underlay = RNifti::asNifti(underlay))) +
    images(list(atlas = RNifti::asNifti(atlas)), labels = lut) +
    slices("k = 1") +
    geom_brain("underlay") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = label), fill_scale = scale_fill_lut(lut)) +
    geom_outline("atlas", mapping = ggplot2::aes(outline = label), outline_scale = scale_fill_lut(lut), show_legend = FALSE)

  patch <- render(p)
  expect_s3_class(patch, "ggbrain_patchwork")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(patchwork::patchworkGrob(patch))

  built <- ggplot2::ggplot_build(patch[[1L]])
  fill_cols <- unique(stats::na.omit(built$data[[2L]]$fill))
  expect_setequal(toupper(fill_cols), c("#FF0000", "#0000FF"))
})
