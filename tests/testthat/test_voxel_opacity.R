test_that("color and opacity use independent aligned maps across slices and fill scales", {
  dims <- c(5, 5, 2)
  effect <- array(rep(c(-2, 2, -1, 1, 0), 10), dims)
  z <- array(rep(c(0, 1, -2, 3, 4, NA), length.out = prod(dims)), dims)
  z[, , 2] <- z[, , 2] / 2
  base <- ggbrain() + images(list(effect = RNifti::asNifti(effect), z = RNifti::asNifti(z))) +
    slices(c("k=1", "k=2")) + define("half_z := z / 2")
  obj <- base +
    geom_brain("effect", mapping = ggplot2::aes(alpha = half_z), alpha_cutoff = 1) +
    geom_brain("effect", fill = "green", mapping = ggplot2::aes(alpha = abs(z) / 4),
               show_legend = FALSE)
  patch <- render(obj)
  expect_no_error(patchwork::patchworkGrob(patch))
  for (i in 1:2) {
    panel <- patch[[i]]
    layers <- ggplot2::ggplot_build(panel)$data
    src <- obj$get_slice_data(slice_index = i)[[1]]
    first <- layers[[1]]
    coords <- c("dim1", "dim2", "value")
    maps <- merge(src$z[coords], src$effect[coords], by = c("dim1", "dim2"),
                  suffixes = c(".z", ".effect"))
    pixels <- dplyr::left_join(first[c("x", "y")], maps, by = c(x = "dim1", y = "dim2"))
    stat <- pixels$value.z
    fill <- pixels$value.effect
    magnitude <- pmin(1, abs(stat) / 2)^2
    visible <- !is.na(stat) & !is.na(fill)
    expect_equal(first$alpha_ggnewscale_1, ifelse(visible & fill < 0, magnitude, 0))
    expect_equal(layers[[2]]$alpha_ggnewscale_1, ifelse(visible & fill > 0, magnitude, 0))
    expect_equal(layers[[3]]$alpha, ifelse(visible, abs(stat) / 4, 0))
    expect_equal(panel$scales$get_scales("alpha_ggnewscale_1")$get_breaks(), c(0, 0.25, 1))
    expect_equal(panel$scales$get_scales("alpha")$guide, "none")
  }
})

test_that("alpha_cutoff alone uses the original layer values and preserves fixed alpha", {
  d <- expand.grid(dim1 = 1:3, dim2 = 1:2)
  d$value <- c(-4, -2, 0, NA, 2, 4)
  make_layer <- function(...) ggbrain_layer_brain$new(definition = "z", data = d,
                                                       fill_scale = ggplot2::scale_fill_gradient2(), ...)
  build <- function(layer) ggplot2::ggplot_build(layer$add_to_gg(ggplot2::ggplot()))$data[[1]]
  expect_equal(build(make_layer(alpha_cutoff = 4))$alpha, c(1, 0.25, 0, 0, 0.25, 1))
  expect_equal(unique(build(make_layer(alpha = 0.4))$alpha), 0.4)
  expect_error(make_layer(alpha = 0.5, alpha_cutoff = 1), "fixed alpha or voxelwise")
  expect_error(make_layer(alpha_cutoff = 0), "positive")
  expect_error(make_layer(mapping = ggplot2::aes(alpha = abs(value))), "between 0 and 1")
})

test_that("aligned opacity images distinguish zero and missing pixels on the display grid", {
  dims <- c(4, 4, 2)
  effect <- array(2, dims)
  weights <- array(rep(c(0, 0.25, 1, NA_real_), 8), dims)
  base <- ggbrain() + images(list(effect = RNifti::asNifti(effect), weight = RNifti::asNifti(weights))) +
    slices("k=1") + geom_brain("effect", mapping = ggplot2::aes(alpha = 1 - .data$weight))
  alpha <- function(p) {
    built <- ggplot2::ggplot_build(render(p)[[1L]])$data[[1L]]
    built$alpha[built$y == 1L][order(built$x[built$y == 1L])]
  }

  expect_equal(alpha(base), c(1, 0.75, 0, 0))
  expect_true(is.na(base$get_slice_data("weight")[[1L]]$value[1L]))
  expect_equal(alpha(base + target_resolution(0.5, interpolation = "nearest")),
               rep(c(1, 0.75, 0, 0), each = 2))
  cubic <- alpha(base + target_resolution(0.5, interpolation = "cubic"))
  expect_equal(cubic[c(1L, 8L)], c(1, 0))
  expect_true(all(cubic >= 0 & cubic <= 1))
})

test_that("the falloff power controls both rendered voxels and the statistic key", {
  z <- array(rep(c(-6, -4, -2, 0, 2, 4, 6, NA_real_), 4), c(8, 2, 2))
  z[, , 2] <- z[, , 2] / 2
  base <- ggbrain() + images(list(effect = RNifti::asNifti(array(1, dim(z))),
                                  z = RNifti::asNifti(z))) + slices(c("k=1", "k=2"))
  for (power in c(0.5, 1, 3)) {
    panels <- render(base + geom_brain("effect", mapping = ggplot2::aes(alpha = z),
                                       alpha_cutoff = 4, alpha_power = power))
    for (i in 1:2) {
      pixels <- ggplot2::ggplot_build(panels[[i]])$data[[1L]]
      magnitude <- abs(z[cbind(pixels$x, pixels$y, i)])
      expected <- ifelse(is.na(magnitude), 0, pmin(magnitude / 4, 1)^power)
      expect_equal(pixels$alpha, expected)
      key <- panels[[i]]$scales$get_scales("alpha")
      expect_equal(key$get_breaks(), c(0, 0.5^power, 1))
      expect_equal(key$get_labels(), c("0", "2", "\u22654"))
    }
  }
})

test_that("custom falloff requires a cutoff and a positive finite exponent", {
  for (bad in list(0, -1, Inf, NA_real_, c(1, 2))) {
    expect_error(geom_brain("effect", alpha_cutoff = 3, alpha_power = bad), "alpha_power")
  }
  expect_error(geom_brain("effect", mapping = ggplot2::aes(alpha = weight), alpha_power = 3),
               "alpha_cutoff")
})

test_that("bisided and repeated layers draw one swatch per opacity legend entry", {
  d <- expand.grid(dim1 = 1:2, dim2 = 1:2)
  d$value <- c(-4, -2, 2, 4)
  layer <- ggbrain_layer_brain$new(definition = "z", data = d, fill_scale = scale_fill_bisided(),
                                   alpha_cutoff = 4, alpha_power = 3)
  p <- layer$add_to_gg(layer$add_to_gg(ggplot2::ggplot())) +
    ggplot2::theme(legend.key = ggplot2::element_blank())
  expect_no_warning(plot_table <- ggplot2::ggplotGrob(p))
  box <- plot_table$grobs[[which(plot_table$layout$name == "guide-box-right")]]
  guides <- box$grobs[box$layout$name == "guides"]
  # Only the opacity guides draw discrete keys. Each layer gets its own guide.
  alpha_guides <- Filter(function(g) any(startsWith(g$layout$name, "key-")), guides)
  expect_length(alpha_guides, 2L)
  for (guide in alpha_guides) {
    keys <- guide$grobs[startsWith(guide$layout$name, "key-")]
    expect_length(keys, 3L)
    for (i in seq_along(keys)) {
      swatches <- Filter(function(child) inherits(child, "rect"), keys[[i]]$children)
      expect_length(swatches, 1L)
      alpha <- grDevices::col2rgb(swatches[[1L]]$gp$fill, alpha = TRUE)[4L, 1L]
      expect_equal(unname(alpha), round(255 * c(0, 0.5^3, 1)[i]))
    }
  }
})

test_that("opacity uses the imputed fill value or the separate statistic after hole filling", {
  dims <- c(5, 5, 2)
  effect <- array(2, dims)
  effect[3, 3, ] <- NA_real_
  statistic <- array(1, dims)
  statistic[3, 3, ] <- 3
  base <- ggbrain() + images(list(effect = RNifti::asNifti(effect), z = RNifti::asNifti(statistic))) +
    slices("k=1")
  center <- function(...) {
    p <- base + geom_brain("effect", fill_holes = 1L, alpha_cutoff = 4, ...)
    d <- ggplot2::ggplot_build(render(p)[[1L]])$data[[1L]]
    d$alpha[d$x == 3L & d$y == 3L]
  }

  expect_equal(center(), 0.25)
  expect_equal(center(mapping = ggplot2::aes(alpha = z)), (3 / 4)^2)
})

test_that("reassigning an existing layer mapping updates and clears voxel alpha", {
  d <- expand.grid(dim1 = 1:2, dim2 = 1:2)
  d$value <- 1:4
  d$weight <- c(0, 0.25, 0.75, 1)
  layer <- ggbrain_layer_brain$new(definition = "effect", data = d)
  build <- function() ggplot2::ggplot_build(layer$add_to_gg(ggplot2::ggplot()))$data[[1L]]

  layer$mapping <- ggplot2::aes(alpha = weight)
  expect_equal(build()$alpha, d$weight)
  layer$mapping <- ggplot2::aes(alpha = 1 - weight)
  expect_equal(build()$alpha, 1 - d$weight)
  expect_error(layer$mapping <- ggplot2::aes(alpha = value), "between 0 and 1")
  expect_equal(build()$alpha, 1 - d$weight)
  layer$mapping <- ggplot2::aes(fill = value)
  expect_true(all(is.na(build()$alpha)))
})

test_that("remapping numeric fill updates cutoff opacity and its legend together", {
  d <- expand.grid(dim1 = 1:2, dim2 = 1:2)
  d$value <- 1:4
  d$beta <- 2
  d$t_stat <- c(-4, 1, 0, NA_real_)
  layer <- ggbrain_layer_brain$new(definition = "atlas", data = d,
                                   mapping = ggplot2::aes(fill = beta),
                                   alpha_cutoff = 4, alpha_power = 1)

  plot <- layer$add_to_gg(ggplot2::ggplot())
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$alpha, rep(0.5, 4))
  layer$mapping <- ggplot2::aes(fill = t_stat)
  plot <- layer$add_to_gg(ggplot2::ggplot())
  expect_equal(ggplot2::ggplot_build(plot)$data[[1L]]$alpha, c(1, 0.25, 0, 0))
  expect_identical(plot$scales$get_scales("alpha")$name, "|t_stat| (power 1)")
})

test_that("two-sided fill scales stay finite when one sign is absent", {
  d <- expand.grid(dim1 = 1:2, dim2 = 1:2)
  for (sign in c(-1, 1)) {
    d$value <- sign * (2:5)
    layer <- ggbrain_layer_brain$new(definition = "effect", data = d,
                                     fill_scale = scale_fill_bisided(), unify_scales = FALSE,
                                     alpha_cutoff = 4)
    expect_no_warning(g <- layer$add_to_gg(ggplot2::ggplot()))
    expect_equal(g$scales$scales[[1L]]$limits, c(-5, -2))
    expect_equal(g$scales$scales[[2L]]$limits, c(2, 5))
  }
})
