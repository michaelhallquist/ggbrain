test_that("native 3D labeling implements face, edge, and corner connectivity", {
  face_mask <- array(FALSE, dim = c(4, 4, 4))
  face_mask[2, 2, 2] <- TRUE
  face_mask[3, 2, 2] <- TRUE

  edge_mask <- array(FALSE, dim = c(4, 4, 4))
  edge_mask[2, 2, 2] <- TRUE
  edge_mask[3, 3, 2] <- TRUE

  corner_mask <- array(FALSE, dim = c(4, 4, 4))
  corner_mask[2, 2, 2] <- TRUE
  corner_mask[3, 3, 3] <- TRUE

  expect_length(unique(label_3d_components(face_mask, 1L)[face_mask]), 1L)
  expect_length(unique(label_3d_components(edge_mask, 1L)[edge_mask]), 2L)
  expect_length(unique(label_3d_components(edge_mask, 2L)[edge_mask]), 1L)
  expect_length(unique(label_3d_components(corner_mask, 2L)[corner_mask]), 2L)
  expect_length(unique(label_3d_components(corner_mask, 3L)[corner_mask]), 1L)
})

test_that("native 3D labeling handles empty, full, and large connected volumes", {
  empty <- array(FALSE, dim = c(3, 4, 5))
  full <- array(TRUE, dim = c(3, 4, 5))
  large <- array(TRUE, dim = c(60, 50, 40))

  expect_identical(label_3d_components(empty, 3L), array(0L, dim = dim(empty)))
  expect_identical(label_3d_components(full, 1L), array(1L, dim = dim(full)))

  large_result <- find_3d_clusters(large, nn = 3L)
  expect_identical(large_result$size, length(large))
  expect_equal(
    unname(unlist(large_result[1L, c("com_i", "com_j", "com_k")])),
    c(30.5, 25.5, 20.5)
  )
})

test_that("native labels exactly match the previous R implementation", {
  set.seed(20260611)
  masks <- c(
    list(
      array(FALSE, dim = c(4, 5, 3)),
      array(TRUE, dim = c(4, 5, 3))
    ),
    lapply(c(0.05, 0.15, 0.35, 0.65), function(probability) {
      array(runif(7 * 6 * 5) < probability, dim = c(7, 6, 5))
    })
  )

  for (mask in masks) {
    for (nn in 1:3) {
      expect_identical(
        label_3d_components(mask, nn),
        label_3d_components_r_reference(mask, nn),
        info = paste("nn =", nn, "foreground =", sum(mask))
      )
    }
  }
})

test_that("native cluster statistics and filtering match the previous R implementation", {
  set.seed(1138)
  masks <- lapply(c(0.08, 0.2, 0.45), function(probability) {
    array(runif(8 * 7 * 6) < probability, dim = c(8, 7, 6))
  })

  for (mask in masks) {
    for (nn in 1:3) {
      for (min_size in c(1L, 2L, 4L, 10L)) {
        native <- find_3d_clusters(mask, min_size, nn, return_labels = TRUE)
        reference <- find_3d_clusters_r_reference(mask, min_size, nn, return_labels = TRUE)

        expect_equal(
          native,
          reference,
          tolerance = 1e-12,
          info = paste("nn =", nn, "min_size =", min_size, "foreground =", sum(mask))
        )
        expect_identical(
          attr(native, "labeled_volume"),
          attr(reference, "labeled_volume"),
          info = paste("labels: nn =", nn, "min_size =", min_size)
        )
      }
    }
  }
})

test_that("native bisided clustering preserves sign separation and ID ordering", {
  set.seed(8675309)
  values <- array(sample(c(-3, -2, 0, 2, 3), 9 * 8 * 7, replace = TRUE), dim = c(9, 8, 7))
  mask <- abs(values) > 1

  for (nn in 1:3) {
    for (min_size in c(1L, 3L, 8L)) {
      native <- find_bisided_3d_clusters(mask, values, min_size, nn, return_labels = TRUE)
      reference <- find_bisided_3d_clusters_r_reference(mask, values, min_size, nn, return_labels = TRUE)

      expect_equal(native, reference, tolerance = 1e-12)
      expect_identical(attr(native, "labeled_volume"), attr(reference, "labeled_volume"))
      if (nrow(native) > 0L) {
        expect_identical(native$cluster_id, seq_len(nrow(native)))
        expect_false(any(vapply(seq_len(nrow(native)), function(ii) {
          cluster_values <- values[attr(native, "labeled_volume") == native$cluster_id[ii]]
          any(cluster_values > 0) && any(cluster_values < 0)
        }, logical(1L))))
      }
    }
  }
})

test_that("native 3D labeling validates its inputs and optional label output", {
  mask <- array(FALSE, dim = c(2, 2, 2))

  expect_error(label_3d_components_cpp(mask, nn = 0L), "nn must be 1, 2, or 3")
  expect_error(label_3d_components_cpp(mask, min_size = 0L), "min_size")
  expect_error(label_3d_components_cpp(matrix(FALSE, 2, 2)), "3D array")

  mask[1, 1, 1] <- NA
  expect_error(label_3d_components_cpp(mask), "missing")

  result <- label_3d_components_cpp(array(TRUE, dim = c(2, 2, 2)), return_labels = FALSE)
  expect_null(result$labels)
  expect_identical(result$clusters$size, 8L)
})
