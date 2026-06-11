test_that("cluster_slices outline uses base image header and preserves cluster ids", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  img <- ggbrain_images$new(images = c(underlay = underlay, overlay = overlay))
  spec <- cluster_slices(
    definition = "overlay[abs(overlay) > 3]",
    nclusters = 4,
    min_clust_size = 40,
    plane = "axial",
    outline = TRUE
  )

  res <- resolve_cluster_slices(spec, img)

  # voxel->world transform should use the overlay header, not the first image
  expected_world <- RNifti::voxelToWorld(
    as.matrix(res$cluster_data[, c("com_i", "com_j", "com_k")]),
    img$get_headers("overlay", drop = TRUE)
  )
  expect_equal(res$cluster_data$com_z, unname(expected_world[, 3]), tolerance = 1e-6)

  # palette and labeled ids should align with top clusters
  expect_length(res$outline_palette, nrow(res$cluster_data))
  outline_ids <- sort(unique(as.integer(res$labeled_volume[!is.na(res$labeled_volume)])))
  expect_equal(outline_ids, seq_len(nrow(res$cluster_data)))
})

test_that("underlay keeps grayscale scale when selected slices contain tiny negative values", {
  arr <- array(1, dim = c(5, 5, 5))
  arr[1, 1, 5] <- -0.001

  tf <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tf), add = TRUE)
  RNifti::writeNifti(arr, tf)

  gg_obj <- ggbrain() +
    images(c(underlay = tf)) +
    slices("z = 100%") +
    geom_brain("underlay")

  patch <- gg_obj$render()
  fill_scales <- patch$scales$scales

  expect_false(any(vapply(fill_scales, inherits, logical(1), "ScaleBisided")))
  expect_equal(fill_scales[[1]]$palette(c(0, 1)), c("#141414", "#EBEBEB"))
})

test_that("cluster_slices outline renders with multiple cluster ids across slices", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 3]",
      nclusters = 4,
      min_clust_size = 40,
      plane = "axial",
      outline = TRUE,
      outline_size = 2L
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay", name = "Activation")

  patch <- gg_obj$render()
  expect_s3_class(patch, "ggbrain_patchwork")

  outline_data <- gg_obj$get_slice_data(image_name = "cluster_outline", as_matrix = FALSE)
  all_vals <- sort(unique(unlist(lapply(outline_data, function(df) df$value))))
  all_vals <- all_vals[!is.na(all_vals)]

  # union across slices should retain all cluster ids
  expect_setequal(all_vals, seq_len(4))
})

test_that("cluster_slices outline validates color/scale exclusivity", {
  expect_error(
    cluster_slices(definition = "overlay", outline = TRUE, outline_color = c("a", "b")),
    "must be a single value"
  )
  expect_error(
    cluster_slices(definition = "overlay", outline = TRUE, outline_color = "red", outline_scale = ggplot2::scale_fill_manual(values = c("1" = "red"))),
    "mutually exclusive"
  )
  expect_error(
    cluster_slices(definition = "overlay", outline = TRUE, outline_show_legend = c(TRUE, FALSE)),
    "length 1"
  )
})

test_that("cluster_slices outline respects user-provided outline_scale", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  custom_scale <- ggplot2::scale_fill_manual(
    values = c("1" = "red", "2" = "blue", "3" = "green", "4" = "black"),
    limits = c("1", "2", "3", "4")
  )

  gg_obj <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 3]",
      nclusters = 4,
      min_clust_size = 40,
      plane = "axial",
      outline = TRUE,
      outline_size = 2L,
      outline_scale = custom_scale
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay", name = "Activation")

  gg_obj$render()

  outline_layers <- Filter(function(l) inherits(l, "ggbrain_layer_outline"), gg_obj$ggb_plot$layers)
  expect_true(length(outline_layers) >= 1L)
  ol <- outline_layers[[length(outline_layers)]]

  expect_true(inherits(ol$fill_scale, "Scale"))
  expect_equal(ol$fill_scale$limits, custom_scale$limits)
  expect_false("outline_color" %in% names(gg_obj$ggb_cluster_data[[1]])) # no auto palette assigned
})

test_that("cluster_slices outline honors outline_show_legend", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  # suppress legend
  gg_off <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 3]",
      nclusters = 2,
      min_clust_size = 40,
      plane = "axial",
      outline = TRUE,
      outline_show_legend = FALSE
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay", name = "Activation")
  gg_off$render()
  outline_layers_off <- Filter(function(l) inherits(l, "ggbrain_layer_outline"), gg_off$ggb_plot$layers)
  expect_false(outline_layers_off[[length(outline_layers_off)]]$show_legend)

  # force legend on even with single cluster
  gg_on <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 6]",
      nclusters = 2,
      min_clust_size = 10,
      plane = "axial",
      outline = TRUE,
      outline_show_legend = TRUE
    )) +
    geom_brain("underlay") +
    geom_brain(definition = "overlay", name = "Activation")
  gg_on$render()
  outline_layers_on <- Filter(function(l) inherits(l, "ggbrain_layer_outline"), gg_on$ggb_plot$layers)
  expect_true(outline_layers_on[[length(outline_layers_on)]]$show_legend)
})

test_that("cluster_slices outline with user scale keeps all cluster ids across slices", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[overlay > 2]",
      nclusters = 4,
      min_clust_size = 20,
      plane = "axial",
      outline = TRUE,
      outline_scale = ggplot2::scale_fill_brewer(palette = "Dark2"),
      outline_size = 2
    )) +
    geom_brain("underlay") +
    geom_brain("overlay")

  gg_obj$render()

  outline_layers <- Filter(function(l) inherits(l, "ggbrain_layer_outline"), gg_obj$ggb_plot$layers)
  ol <- outline_layers[[length(outline_layers)]]

  expect_equal(ol$fill_scale$limits, as.character(1:4))
  expect_false(isTRUE(ol$fill_scale$drop))
})

test_that("compute_cluster_slices warns when fewer clusters than requested", {
  arr <- array(1, dim = c(3, 3, 3)) # single mega-cluster
  tf <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tf), add = TRUE)
  RNifti::writeNifti(arr, tf)

  expect_warning(
    res <- compute_cluster_slices(
      images = c(single = tf),
      definition = "single",
      nclusters = 4,
      min_clust_size = 1,
      plane = "axial",
      nn = 3,
      outline = TRUE,
      outline_color = NULL,
      outline_size = 1L,
      outline_scale = NULL,
      outline_show_legend = NULL
    ),
    "Only 1 cluster"
  )

  expect_equal(nrow(res$cluster_data), 1L)
  expect_true(is.integer(res$labeled_volume))
})

test_that("cluster_slices defaults to bisided clustering", {
  arr <- array(0, dim = c(5, 5, 5))
  arr[2:4, 3, 3] <- c(3.1, -3.2, 3.3)

  tf <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tf), add = TRUE)
  RNifti::writeNifti(arr, tf)

  res <- compute_cluster_slices(
    images = c(overlay = tf),
    definition = "overlay[abs(overlay) > 2.5]",
    nclusters = 3,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    outline = TRUE,
    outline_color = NULL,
    outline_size = 1L,
    outline_scale = NULL,
    outline_show_legend = NULL
  )

  expect_equal(nrow(res$cluster_data), 3L)
  expect_equal(sort(res$cluster_data$size), c(1L, 1L, 1L))
  side_counts <- table(res$cluster_data$side)
  expect_equal(as.integer(side_counts[c("negative", "positive")]), c(1L, 2L))
  expect_equal(length(unique(as.vector(res$labeled_volume[2:4, 3, 3]))), 3L)
})

test_that("two_sided clustering preserves historical sign-agnostic connections", {
  arr <- array(0, dim = c(5, 5, 5))
  arr[2:4, 3, 3] <- c(3.1, -3.2, 3.3)

  tf <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(tf), add = TRUE)
  RNifti::writeNifti(arr, tf)

  res <- compute_cluster_slices(
    images = c(overlay = tf),
    definition = "overlay[abs(overlay) > 2.5]",
    nclusters = 1,
    min_clust_size = 1,
    plane = "sagittal",
    nn = 1,
    sided = "two_sided",
    outline = TRUE,
    outline_color = NULL,
    outline_size = 1L,
    outline_scale = NULL,
    outline_show_legend = NULL
  )

  expect_equal(nrow(res$cluster_data), 1L)
  expect_equal(res$cluster_data$size, 3L)
  expect_equal(res$cluster_data$side, "two_sided")
  expect_equal(unique(as.vector(res$labeled_volume[2:4, 3, 3])), 1L)
})

test_that("apply_definition_3d supports cross-image conditions", {
  base_arr <- array(1, dim = c(3, 3, 3))
  overlay_arr <- array(rep(c(-2, 2), length.out = 27), dim = c(3, 3, 3))

  tf_base <- tempfile(fileext = ".nii.gz")
  tf_overlay <- tempfile(fileext = ".nii.gz")
  on.exit(unlink(c(tf_base, tf_overlay)), add = TRUE)
  RNifti::writeNifti(base_arr, tf_base)
  RNifti::writeNifti(overlay_arr, tf_overlay)

  expect_warning(compute_cluster_slices(
    images = c(feedback = tf_base, overlay = tf_overlay),
    definition = "feedback[overlay > 0]",
    nclusters = 2,
    min_clust_size = 1,
    plane = "axial",
    nn = 3,
    outline = FALSE,
    outline_color = NULL,
    outline_size = 1L,
    outline_scale = NULL,
    outline_show_legend = NULL
  ), "Only 1 cluster")
})

test_that("slice_to_outline preserves group key columns", {
  layer <- ggbrain_layer_outline$new(
    definition = "test_outline",
    mapping = ggplot2::aes(outline = network),
    size = 1L
  )

  df <- data.frame(
    dim1 = c(1L, 1L, 2L, 2L),
    dim2 = c(1L, 2L, 1L, 2L),
    value = c(1L, 1L, 1L, NA_integer_),
    network = c("netA", "netA", "netA", "netA")
  )

  res <- layer$.__enclos_env__$private$slice_to_outline(df, group_cols = c("network"), blur_sigma = NULL, dil_ero = 0L)
  expect_true("network" %in% names(res))
  expect_true(all(na.omit(res$network) == "netA"))
})

test_that("outline layer errors when requested grouping column is missing", {
  layer <- ggbrain_layer_outline$new(
    definition = "test_outline",
    mapping = ggplot2::aes(outline = value, group = roi),
    size = 1L
  )

  df <- data.frame(
    dim1 = c(1L, 2L, 2L),
    dim2 = c(1L, 1L, 2L),
    value = c(1L, 1L, 2L)
  )

  suppressWarnings(layer$data <- df)
  expect_error(
    suppressWarnings(layer$.__enclos_env__$private$get_plot_data()),
    "group_cols"
  )
})

test_that("repeated cluster slice renders do not mutate slices or layers", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(cluster_slices(
      definition = "overlay[abs(overlay) > 3]",
      nclusters = 4,
      min_clust_size = 40,
      plane = "axial",
      outline = TRUE
    )) +
    geom_brain("underlay") +
    geom_brain("overlay")

  specification_layer_names <- vapply(gg_obj$ggb_layers, `[[`, character(1L), "name")
  specification_layer_defs <- vapply(gg_obj$ggb_layers, `[[`, character(1L), "definition")
  expect_null(gg_obj$ggb_slices)

  gg_obj$render()
  first_coords <- gg_obj$ggb_plot$slices$coord_input
  first_render_layers <- vapply(gg_obj$ggb_plot$layers, `[[`, character(1L), "name")
  first_cluster_data <- gg_obj$get_cluster_data()

  expect_null(gg_obj$ggb_slices)
  expect_identical(vapply(gg_obj$ggb_layers, `[[`, character(1L), "name"), specification_layer_names)
  expect_identical(vapply(gg_obj$ggb_layers, `[[`, character(1L), "definition"), specification_layer_defs)
  expect_length(first_render_layers, length(specification_layer_names) + 1L)

  gg_obj$render()

  expect_identical(gg_obj$ggb_plot$slices$coord_input, first_coords)
  expect_identical(vapply(gg_obj$ggb_plot$layers, `[[`, character(1L), "name"), first_render_layers)
  expect_equal(gg_obj$get_cluster_data(), first_cluster_data)
  expect_null(gg_obj$ggb_slices)
  expect_identical(vapply(gg_obj$ggb_layers, `[[`, character(1L), "name"), specification_layer_names)
  expect_identical(vapply(gg_obj$ggb_layers, `[[`, character(1L), "definition"), specification_layer_defs)

  # Changes to the stored specification must be picked up by the next render.
  gg_obj$ggb_layers[[2L]]$alpha <- 0.35
  gg_obj$ggb_cluster_slices[[1L]]$outline_size <- 3L
  gg_obj$render()

  expect_equal(gg_obj$ggb_plot$layers[[2L]]$alpha, 0.35)
  outline_layers <- Filter(
    function(layer) inherits(layer, "ggbrain_layer_outline"),
    gg_obj$ggb_plot$layers
  )
  expect_equal(outline_layers[[length(outline_layers)]]$size, 3L)
})
