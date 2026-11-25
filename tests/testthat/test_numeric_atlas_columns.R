expect_silent_render_grob <- function(patch) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(patchwork::patchworkGrob(patch))
}

make_synthetic_atlas_setup <- function() {
  dims <- c(6, 6, 3)
  underlay <- array(50, dim = dims)
  atlas <- array(0, dim = dims)

  # Slice 1: parcels 1 and 2 (both negative beta)
  atlas[2:3, 2:3, 1] <- 1
  atlas[4:5, 2:3, 1] <- 2

  # Slice 2: parcels 3 and 4 (positive beta, and parcel 4 has beta = 0)
  atlas[2:3, 2:3, 2] <- 3
  atlas[4:5, 2:3, 2] <- 4

  # Slice 3: parcels 5 (positive beta) and 6 (missing from stats table -> NA)
  atlas[2:3, 2:3, 3] <- 5
  atlas[4:5, 2:3, 3] <- 6

  roi_stats <- data.frame(
    value = c(1, 2, 3, 4, 5),
    region = c("ROI_1", "ROI_2", "ROI_3", "ROI_4", "ROI_5"),
    network = c("NetA", "NetA", "NetB", "NetB", "NetC"),
    beta = c(-3.0, -1.0, 2.0, 0.0, 4.0),
    t_stat = c(-4.0, -1.5, 2.5, 0.2, 5.0),
    p_fdr = c(0.005, 0.12, 0.02, 0.80, 0.001),
    stringsAsFactors = FALSE
  )

  list(
    underlay = RNifti::asNifti(underlay),
    atlas = RNifti::asNifti(atlas),
    roi_stats = roi_stats
  )
}

test_that("numeric ROI columns unify scale limits and auto-select bisided scales across slices", {
  setup <- make_synthetic_atlas_setup()

  # Default scale with mixed signs across slices (slice 1 all neg, slice 2/3 pos)
  gg_bisided <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2", "k = 3")) +
    geom_brain("underlay") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = beta), unify_scales = TRUE)

  patch_bisided <- render(gg_bisided)
  expect_silent_render_grob(patch_bisided)

  panels <- gg_bisided$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  atlas_layers <- lapply(panels, function(pn) pn$get_layers()[[2L]])
  expect_true(all(vapply(atlas_layers, function(l) isTRUE(l$bisided), logical(1L))))
  expect_true(all(vapply(atlas_layers, function(l) !isTRUE(l$categorical_fill), logical(1L))))

  # The positive scale includes the measured zero, with symmetric negative limits.
  for (lay in atlas_layers) {
    built <- ggplot2::ggplot_build(lay$add_to_gg(ggplot2::ggplot()))
    neg_lims <- built$plot$scales$scales[[1L]]$limits
    pos_lims <- built$plot$scales$scales[[2L]]$limits
    expect_equal(neg_lims, c(-4, 0))
    expect_equal(pos_lims, c(0, 4))
  }

  # Single diverging scale: unified limits must use beta (-3..4), not parcel IDs (1..6)
  gg_div <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2", "k = 3")) +
    geom_brain("underlay") +
    geom_brain(
      "atlas",
      mapping = ggplot2::aes(fill = beta),
      fill_scale = ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red"),
      unify_scales = TRUE
    )

  patch_div <- render(gg_div)
  expect_silent_render_grob(patch_div)

  div_panels <- gg_div$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  for (pn in div_panels) {
    lay <- pn$get_layers()[[2L]]
    expect_equal(unname(lay$fill_scale$limits), c(-3, 4))
  }

  # Slice 2 parcel 4 has beta == 0 and should remain finite (not masked to NA)
  s2_data <- div_panels[[2L]]$get_layers()[[2L]]$data
  p4_betas <- unique(s2_data$beta[which(s2_data$value == 4)])
  expect_equal(p4_betas, 0)
})

test_that("numeric-only lookup tables (without character label columns) render cleanly", {
  setup <- make_synthetic_atlas_setup()
  numeric_only <- setup$roi_stats[, c("value", "beta", "p_fdr")]

  gg_obj <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = numeric_only) +
    slices(c("k = 1", "k = 2")) +
    geom_brain("underlay") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = beta))

  patch <- render(gg_obj)
  expect_silent_render_grob(patch)

  # Ensure label_columns is NULL and table_columns retains beta & p_fdr
  slc_df <- gg_obj$get_slice_data("atlas")[[1L]]
  expect_null(attr(slc_df, "label_columns"))
  expect_setequal(attr(slc_df, "table_columns"), c("beta", "p_fdr"))

  # Strictly negative slice gets Blues scale when unify_scales = FALSE
  gg_neg <- ggbrain() +
    images(list(atlas = setup$atlas), labels = numeric_only) +
    slices("k = 1") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = beta), unify_scales = FALSE)
  expect_silent_render_grob(render(gg_neg))
  neg_layer <- gg_neg$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels[[1L]]$get_layers()[[1L]]
  expect_false(neg_layer$bisided)
  expect_identical(neg_layer$fill_scale$palette(2)[1L],
                   ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)$palette(2)[1L])
})

test_that("contrasts and subsets can filter and compute on numeric ROI columns without treating NA as 0", {
  setup <- make_synthetic_atlas_setup()

  # Subset by p_fdr < 0.05: parcels 1, 3, 5 survive; parcel 2 (0.12), 4 (0.80), and 6 (NA!) are excluded
  gg_sig <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2", "k = 3")) +
    geom_brain("underlay") +
    geom_brain(
      "atlas[atlas.p_fdr < 0.05]",
      mapping = ggplot2::aes(fill = beta),
      fill_scale = ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red")
    ) +
    geom_outline("atlas[atlas.p_fdr < 0.05]", outline = "black")

  patch_sig <- render(gg_sig)
  expect_silent_render_grob(patch_sig)

  panels <- gg_sig$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  # Slice 1: only parcel 1 survives
  expect_equal(sort(unique(stats::na.omit(panels[[1L]]$get_layers()[[2L]]$data$value))), 1)
  # Slice 2: only parcel 3 survives
  expect_equal(sort(unique(stats::na.omit(panels[[2L]]$get_layers()[[2L]]$data$value))), 3)
  # Slice 3: only parcel 5 survives (parcel 6 had p_fdr = NA and MUST NOT pass < 0.05)
  expect_equal(sort(unique(stats::na.omit(panels[[3L]]$get_layers()[[2L]]$data$value))), 5)

  # Unified limits reflect surviving parcels' beta (-3 to 4)
  expect_equal(unname(panels[[1L]]$get_layers()[[2L]]$fill_scale$limits), c(-3, 4))

  # Direct contrast expression on a numeric column + case_when classification
  gg_direct <- ggbrain() +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2", "k = 3")) +
    define(sig_beta := atlas.beta[atlas.p_fdr < 0.05]) +
    define(direction := case_when(
      atlas.p_fdr < 0.05 & atlas.beta > 0 ~ "Positive",
      atlas.p_fdr < 0.05 & atlas.beta < 0 ~ "Negative"
    )) +
    geom_brain("sig_beta")

  patch_direct <- render(gg_direct)
  expect_silent_render_grob(patch_direct)

  direct_panels <- gg_direct$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  expect_equal(sort(unique(stats::na.omit(direct_panels[[1L]]$get_layers()[[1L]]$data$value))), -3)
  expect_equal(sort(unique(stats::na.omit(direct_panels[[2L]]$get_layers()[[1L]]$data$value))), 2)
  expect_equal(sort(unique(stats::na.omit(direct_panels[[3L]]$get_layers()[[1L]]$data$value))), 4)

  # Check case_when direction classification excludes NA parcel 6
  dir_s3 <- gg_direct$get_slice_data("direction", slice_index = 3L)[[1L]]
  expect_equal(sort(unique(as.character(stats::na.omit(dir_s3$label)))), "Positive")
  # Verify parcel 6 pixels are NA in direction
  atlas_s3 <- gg_direct$get_slice_data("atlas", slice_index = 3L)[[1L]]
  p6_rows <- which(atlas_s3$value == 6)
  expect_true(all(is.na(dir_s3$label[p6_rows])))
})

test_that("voxelwise opacity and alpha_cutoff work with numeric ROI table columns", {
  setup <- make_synthetic_atlas_setup()

  # 1) Explicit aes(alpha = t_stat) with alpha_cutoff
  gg_alpha_col <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2")) +
    geom_brain("underlay") +
    geom_brain(
      "atlas",
      mapping = ggplot2::aes(fill = beta, alpha = t_stat),
      fill_scale = ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red"),
      alpha_cutoff = 4,
      alpha_power = 2
    )

  patch_alpha <- render(gg_alpha_col)
  expect_silent_render_grob(patch_alpha)

  # Slice 1 has parcels 1 (t=-4 -> alpha=1) and 2 (t=-1.5 -> alpha=(1.5/4)^2)
  b1 <- ggplot2::ggplot_build(patch_alpha[[1L]])$data[[2L]]
  s1 <- gg_alpha_col$get_slice_data("atlas", slice_index = 1L)[[1L]]
  joined1 <- dplyr::left_join(b1[c("x", "y", "alpha")], s1[c("dim1", "dim2", "value", "t_stat")],
                              by = c(x = "dim1", y = "dim2"))
  expect_equal(unique(joined1$alpha[which(joined1$value == 1)]), 1)
  expect_equal(unique(joined1$alpha[which(joined1$value == 2)]), (1.5 / 4)^2)

  # 2) alpha_cutoff alone on aes(fill = beta) thresholds on beta, not parcel IDs
  gg_alpha_default <- ggbrain() +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices("k = 2") +
    geom_brain(
      "atlas",
      mapping = ggplot2::aes(fill = beta),
      fill_scale = ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red"),
      alpha_cutoff = 4,
      alpha_power = 1
    )

  patch_def <- render(gg_alpha_default)
  expect_silent_render_grob(patch_def)

  b2 <- ggplot2::ggplot_build(patch_def[[1L]])$data[[1L]]
  s2 <- gg_alpha_default$get_slice_data("atlas", slice_index = 1L)[[1L]]
  joined2 <- dplyr::left_join(b2[c("x", "y", "alpha")], s2[c("dim1", "dim2", "value", "beta")],
                              by = c(x = "dim1", y = "dim2"))
  # Parcel 3 has beta = 2 (alpha = 2/4 = 0.5); parcel 4 has beta = 0 (alpha = 0)
  expect_equal(unique(joined2$alpha[which(joined2$value == 3)]), 0.5)
  expect_equal(unique(joined2$alpha[which(joined2$value == 4)]), 0)
  expect_identical(patch_def[[1L]]$scales$get_scales("alpha")$name, "|beta| (power 1)")
})

test_that("fill_holes preserves numeric ROI columns and uses mode imputation on parcel IDs", {
  mat <- matrix(1, nrow = 5, ncol = 5)
  mat[4:5, ] <- 2
  mat[2, 2] <- NA # interior hole inside parcel 1

  df <- ggbrain:::mat2df(mat, na_zeros = FALSE)
  df$beta <- ifelse(df$value == 1, -2.5, ifelse(df$value == 2, 3.5, NA_real_))
  attr(df, "table_columns") <- "beta"

  layer <- ggbrain_layer_brain$new(
    name = "atlas",
    definition = "atlas",
    mapping = ggplot2::aes(fill = beta),
    fill_holes = 5
  )
  layer$data <- df

  hole_row <- subset(layer$data, dim1 == 2 & dim2 == 2)
  expect_equal(hole_row$value, 1)
  expect_equal(hole_row$beta, -2.5)
})

test_that("geom_outline maps numeric ROI columns cleanly across signed values", {
  setup <- make_synthetic_atlas_setup()

  gg_out <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices(c("k = 1", "k = 2", "k = 3")) +
    geom_brain("underlay") +
    geom_outline("atlas", mapping = ggplot2::aes(outline = beta), size = 1L)

  patch_out <- render(gg_out)
  expect_silent_render_grob(patch_out)

  panels <- gg_out$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels
  out_layers <- lapply(panels, function(pn) pn$get_layers()[[2L]])
  expect_true(all(vapply(out_layers, function(l) isTRUE(l$bisided), logical(1L))))
})

test_that("target_resolution preserves numeric ROI columns using nearest-neighbor atlas resampling", {
  setup <- make_synthetic_atlas_setup()

  gg_res <- ggbrain() +
    images(list(underlay = setup$underlay)) +
    images(list(atlas = setup$atlas), labels = setup$roi_stats) +
    slices("k = 2") +
    target_resolution(0.5, interpolation = "cubic") +
    geom_brain("underlay") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = beta))

  patch_res <- render(gg_res)
  expect_silent_render_grob(patch_res)

  sdata <- gg_res$get_slice_data("atlas")[[1L]]
  # Only integer parcel IDs 3 and 4 should exist on slice 2 (no interpolated 3.5)
  expect_equal(sort(unique(stats::na.omit(sdata$value))), c(3, 4))
  expect_equal(sort(unique(stats::na.omit(sdata$beta))), c(0, 2))
})

test_that("fill_labels = TRUE preserves numeric table columns alongside filled categorical labels", {
  setup <- make_synthetic_atlas_setup()
  img_obj <- ggbrain_images$new(
    images = list(atlas = setup$atlas),
    labels = list(atlas = setup$roi_stats)
  )

  slc <- img_obj$get_slices("k = 3", fill_labels = TRUE)
  df3 <- slc$slice_data[[1L]]$atlas
  expect_true(all(c("region", "network", "beta", "p_fdr") %in% names(df3)))
  # Parcel 5 gets its joined label and numeric beta; parcel 6 gets fallback "6" and NA beta
  expect_equal(unique(df3$region[which(df3$value == 5)]), "ROI_5")
  expect_equal(unique(df3$beta[which(df3$value == 5)]), 4.0)
  expect_equal(unique(df3$region[which(df3$value == 6)]), "6")
  expect_true(all(is.na(df3$beta[which(df3$value == 6)])))
})

test_that("dotted statistic names and named statistic subsets preserve NA and zero", {
  s <- make_synthetic_atlas_setup()
  tab <- dplyr::rename(s$roi_stats, p.value = p_fdr)
  tab$effect.value <- tab$beta
  g <- ggbrain() + images(list(atlas = s$atlas), labels = tab) +
    slices(c("k=2", "k=3")) +
    define(stat := atlas.effect.value[atlas.p.value < 1]) +
    geom_brain("positive := stat[stat >= 0]") +
    geom_brain("sig := atlas[atlas.p.value < .05]")
  render(g)
  expect_setequal(stats::na.omit(g$get_slice_data("sig")[[2]]$value), 5)
  positive <- g$get_slice_data("positive")
  expect_setequal(positive[[1]]$value, c(0, 2))
  expect_setequal(positive[[2]]$value, c(4, NA_real_))
  expect_false("beta" %in% names(g$get_slice_data("stat")[[1]]))
})

test_that("hole filling on ROI statistics keeps valid zeros and exterior missing pixels", {
  s <- make_synthetic_atlas_setup()
  g <- ggbrain() + images(list(underlay = s$underlay)) +
    images(list(atlas = s$atlas), labels = s$roi_stats) +
    slices("k=2") + geom_brain("stat := atlas.beta", fill_holes = 1L)
  render(g)
  layer <- g$ggb_plot$.__enclos_env__$private$pvt_ggbrain_panels[[1]]$get_layers()[[1]]
  expect_equal(layer$data$value, g$get_slice_data("stat")[[1]]$value)

  # A ring of measured zeros is a valid neighborhood for imputation.
  m <- matrix(NA_real_, 7, 7)
  m[2:6, 2:6] <- 0
  m[4, 4] <- NA_real_
  d <- ggbrain:::mat2df(m)
  attr(d, "measured_statistic") <- TRUE
  layer$data <- d
  m[4, 4] <- 0
  expect_equal(layer$data$value, as.vector(m))
})

test_that("unkeyed lookup statistics cannot color atlas background or affect limits", {
  s <- make_synthetic_atlas_setup()
  tab <- rbind(s$roi_stats, transform(s$roi_stats[1, ], value = NA, beta = 99))
  g <- ggbrain() + images(list(underlay = s$underlay)) +
    images(list(atlas = s$atlas), labels = tab) + slices("k=1") +
    geom_brain("underlay") +
    geom_brain("atlas", mapping = ggplot2::aes(fill = beta))
  p <- render(g)
  d <- g$get_slice_data("atlas")[[1]]
  expect_true(anyNA(d$value))
  expect_true(all(is.na(d$beta[is.na(d$value)])))
  expect_equal(unname(p[[1]]$scales$get_scales("fill")$limits), c(-3, -1))
})

test_that("continuous scales draw measured zeros and use mapped values for sign and limits", {
  s <- make_synthetic_atlas_setup()
  g <- ggbrain() + images(list(atlas = s$atlas), labels = s$roi_stats) +
    slices(c("k=1", "k=2", "k=3"))
  p <- render(g + geom_brain("atlas", mapping = ggplot2::aes(fill = beta)))
  pixel_fill <- function(panel, x, y) {
    raster <- utils::tail(ggplot2::ggplot_build(panel)$data, 1)[[1]]
    raster$fill[raster$x == x & raster$y == y]
  }
  expect_false(pixel_fill(p[[2]], 4, 2) %in% c(NA, "transparent"))
  expect_true(pixel_fill(p[[3]], 4, 2) %in% c(NA, "transparent"))

  for (unify in c(TRUE, FALSE)) {
    p <- render(g + geom_brain("atlas", mapping = ggplot2::aes(fill = abs(beta)),
                               unify_scales = unify))
    expect_length(p[[1]]$layers, 1L)
    expect_false(pixel_fill(p[[1]], 2, 2) %in% c(NA, "transparent"))
    if (unify) expect_equal(unname(p[[1]]$scales$get_scales("fill")$limits), c(0, 4))
  }

  # The negative beta in parcel 1 becomes positive under the mapping. Reversing
  # the positive color scale changes its stored limits, not the sign split.
  signed <- scale_fill_bisided(pos_scale = ggplot2::scale_fill_gradient(transform = "reverse"))
  p <- render(g + geom_brain("atlas", mapping = ggplot2::aes(fill = 2 - beta),
                             fill_scale = signed))
  expect_false(pixel_fill(p[[1]], 2, 2) %in% c(NA, "transparent"))
  expect_equal(unname(p[[1]]$scales$get_scales("fill")$limits), c(-5, 0))

  log_scale <- ggplot2::scale_fill_gradient(transform = "log10")
  p <- render(g + geom_brain("atlas", mapping = ggplot2::aes(fill = abs(beta) + 1),
                             fill_scale = log_scale) +
                geom_brain("atlas", mapping = ggplot2::aes(fill = p_fdr),
                           fill_scale = log_scale))
  expect_null(log_scale$limits)
  for (i in 1:3) {
    scales <- p[[i]]$scales
    expect_equal(unname(scales$get_scales("fill")$limits), log10(c(.001, .8)))
    expect_equal(unname(scales$get_scales("fill_ggnewscale_1")$limits), log10(c(1, 5)))
  }
  expect_false(pixel_fill(p[[1]], 2, 2) %in% c(NA, "transparent"))
})
