label_3d_components_r_reference <- function(mask, nn = 3L) {
  dims <- dim(mask)
  ni <- dims[1L]
  nj <- dims[2L]
  nk <- dims[3L]
  offsets <- get_backward_neighbor_offsets(nn)
  labels <- array(0L, dim = dims)
  next_label <- 1L
  parent <- integer(0)

  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]
      x <- parent[x]
    }
    x
  }

  union_labels <- function(a, b) {
    root_a <- find_root(a)
    root_b <- find_root(b)
    if (root_a != root_b) {
      if (root_a < root_b) {
        parent[root_b] <<- root_a
      } else {
        parent[root_a] <<- root_b
      }
    }
  }

  for (k in seq_len(nk)) {
    for (j in seq_len(nj)) {
      for (i in seq_len(ni)) {
        if (!mask[i, j, k]) next

        neighbor_labels <- integer(0)
        for (oi in seq_len(nrow(offsets))) {
          ni_new <- i + offsets[oi, 1L]
          nj_new <- j + offsets[oi, 2L]
          nk_new <- k + offsets[oi, 3L]

          if (ni_new >= 1L && ni_new <= ni &&
              nj_new >= 1L && nj_new <= nj &&
              nk_new >= 1L && nk_new <= nk &&
              labels[ni_new, nj_new, nk_new] > 0L) {
            neighbor_labels <- c(neighbor_labels, labels[ni_new, nj_new, nk_new])
          }
        }

        if (length(neighbor_labels) == 0L) {
          labels[i, j, k] <- next_label
          parent <- c(parent, next_label)
          next_label <- next_label + 1L
        } else {
          min_label <- min(neighbor_labels)
          labels[i, j, k] <- min_label
          for (neighbor_label in unique(neighbor_labels)) {
            if (neighbor_label != min_label) {
              union_labels(min_label, neighbor_label)
            }
          }
        }
      }
    }
  }

  if (length(parent) > 0L) {
    roots <- vapply(seq_along(parent), find_root, integer(1L))
    unique_roots <- sort(unique(roots))
    label_map <- integer(length(parent))
    label_map[unique_roots] <- seq_along(unique_roots)
    labeled_idx <- which(labels > 0L)
    labels[labeled_idx] <- label_map[roots[labels[labeled_idx]]]
  }

  labels
}

find_3d_clusters_r_reference <- function(mask, min_size = 1L, nn = 3L, return_labels = FALSE) {
  labeled <- label_3d_components_r_reference(mask, nn)
  cluster_ids <- sort(unique(as.vector(labeled)))
  cluster_ids <- cluster_ids[cluster_ids > 0L]

  cluster_stats <- lapply(cluster_ids, function(cluster_id) {
    voxels <- which(labeled == cluster_id, arr.ind = TRUE)
    if (nrow(voxels) < min_size) return(NULL)

    data.frame(
      cluster_id = cluster_id,
      size = nrow(voxels),
      com_i = mean(voxels[, 1L]),
      com_j = mean(voxels[, 2L]),
      com_k = mean(voxels[, 3L])
    )
  })
  cluster_stats <- Filter(Negate(is.null), cluster_stats)

  if (length(cluster_stats) == 0L) {
    cluster_df <- data.frame(
      cluster_id = integer(0),
      size = integer(0),
      com_i = numeric(0),
      com_j = numeric(0),
      com_k = numeric(0)
    )
  } else {
    cluster_df <- do.call(rbind, cluster_stats)
    rownames(cluster_df) <- NULL
  }

  if (isTRUE(return_labels)) {
    labeled[!(labeled %in% cluster_df$cluster_id)] <- 0L
    attr(cluster_df, "labeled_volume") <- labeled
  }
  cluster_df
}

find_bisided_3d_clusters_r_reference <- function(
    mask, values, min_size = 1L, nn = 3L, return_labels = FALSE, zero_tol = 1e-6) {
  labeled <- array(0L, dim = dim(mask))
  next_id <- 1L
  cluster_parts <- list()
  side_masks <- list(
    positive = mask & values > zero_tol,
    negative = mask & values < -zero_tol
  )

  for (side_name in names(side_masks)) {
    side_info <- find_3d_clusters_r_reference(
      side_masks[[side_name]],
      min_size = min_size,
      nn = nn,
      return_labels = return_labels
    )
    if (nrow(side_info) == 0L) next

    old_ids <- side_info$cluster_id
    new_ids <- seq.int(next_id, length.out = nrow(side_info))
    if (isTRUE(return_labels)) {
      side_labeled <- attr(side_info, "labeled_volume")
      for (ii in seq_along(old_ids)) {
        labeled[side_labeled == old_ids[ii]] <- new_ids[ii]
      }
    }

    side_info$cluster_id <- new_ids
    side_info$side <- side_name
    cluster_parts <- c(cluster_parts, list(side_info))
    next_id <- next_id + nrow(side_info)
  }

  if (length(cluster_parts) == 0L) {
    cluster_df <- data.frame(
      cluster_id = integer(0),
      size = integer(0),
      com_i = numeric(0),
      com_j = numeric(0),
      com_k = numeric(0),
      side = character(0)
    )
  } else {
    cluster_df <- do.call(rbind, cluster_parts)
    cluster_df <- cluster_df[, c("cluster_id", "size", "com_i", "com_j", "com_k", "side")]
    rownames(cluster_df) <- NULL
  }

  if (isTRUE(return_labels)) attr(cluster_df, "labeled_volume") <- labeled
  cluster_df
}
