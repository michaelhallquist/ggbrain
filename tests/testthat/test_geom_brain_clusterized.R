test_that("geom_brain_clusterized adds categorical cluster layer without altering slices", {
  underlay <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
  overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")

  gg_obj <- ggbrain() +
    images(c(underlay = underlay, overlay = overlay)) +
    slices(c("z=0")) + # explicit slices; clusterized should not add its own
    geom_brain("underlay") +
    geom_brain_clusterized(
      definition = "overlay[abs(overlay) > 3]",
      name = "PE clusters",
      nclusters = 30,
      min_clust_size = 40,
      fill_scale = ggplot2::scale_fill_manual(values = c("1" = "red", "2" = "blue", "3" = "green")),
      show_legend = TRUE,
      cluster_info = c("number", "voxels"),
      nn = 1
    )

  # render should flag that 4 clusters, not 30, were found
  expect_warning(patch <- gg_obj$render(), regexp = "Only 4 cluster\\(s\\) found")
  expect_s3_class(patch, "ggbrain_patchwork")

  # Ensure slices remain as provided
  expect_equal(length(gg_obj$ggb_plot$slices$coord_input), 1L)

  # Ensure clusterized layer exists with discrete scale
  layers <- gg_obj$ggb_layers
  cl_layers <- Filter(function(l) inherits(l, "ggbrain_layer_brain") && l$name %in% c("clusterized", l$definition), layers)
  expect_true(length(cl_layers) >= 1L)
  cl <- cl_layers[[length(cl_layers)]]
  expect_true(cl$categorical_fill)
  expect_true(is.null(cl$fill_scale) || inherits(cl$fill_scale, "ScaleDiscrete"))
})

test_that("geom_brain_clusterized errors if definition image missing", {
  expect_error({
    obj <- ggbrain() +
      images(c(underlay = system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain"))) +
      geom_brain_clusterized(definition = "missing_image") +
      geom_brain("underlay")
    obj$render()
  }, "not found")
})
