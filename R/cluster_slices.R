#' Find slice locations based on cluster centers of mass in a 3D brain image
#'
#' @description This function identifies connected clusters in a brain image (after applying
#'   an optional definition/contrast) and returns slice coordinates corresponding to the
#'   center of mass of each cluster. This is useful for automatically selecting slices
#'   that display the most relevant activation clusters.
#'
#' @param images A ggbrain_images object or a named character vector of image file paths.
#'   Required for computing cluster locations.
#' @param layer The name of a geom_brain layer whose definition should be used for clustering.
#'   Mutually exclusive with \code{definition}. When using \code{layer}, the function returns
#'   a deferred specification that is resolved during rendering when layer definitions are available.
#' @param definition A definition string (same syntax as geom_brain) specifying the image/contrast
#'   to use for clustering. Examples: \code{"overlay"}, \code{"overlay[overlay > 2.5]"},
#'   \code{"overlay[abs(overlay) > 2.5]"}. Mutually exclusive with \code{layer}.
#' @param nclusters Maximum number of clusters to return slices for. Clusters are sorted by
#'   size (descending), and the top \code{nclusters} are selected. Default: 10
#' @param min_clust_size Minimum cluster size in voxels. Clusters smaller than this are ignored. Default: 1
#' @param plane The plane for slice selection: \code{"axial"} (z), \code{"sagittal"} (x),
#'   or \code{"coronal"} (y). Default: \code{"axial"}
#' @importFrom utils head
#' @param nn Neighborhood connectivity for defining clusters. Must be 1, 2, or 3:
#'   \itemize{
#'     \item \code{nn=1}: 6-connectivity (faces touching only)
#'     \item \code{nn=2}: 18-connectivity (faces and edges touching)
#'     \item \code{nn=3}: 26-connectivity (faces, edges, and corners touching)
#'   }
#'   Default: 3 (most inclusive)
#' @param outline Logical. If \code{TRUE}, a cluster outline mask is generated and automatically
#'   added as a \code{geom_outline()} layer during rendering. Default: \code{FALSE}
#' @param outline_color A single color string for all cluster outlines. If \code{NULL} (default),
#'   distinct colors are assigned to each cluster using the "Dark 3" HCL palette. Mutually
#'   exclusive with \code{outline_scale}.
#' @param outline_size Integer specifying the outline thickness in pixels. Default: 1
#' @param outline_scale A ggplot2 scale object (e.g., \code{scale_fill_manual}) to use for
#'   cluster outline colors. Mutually exclusive with \code{outline_color}.
#' @param outline_show_legend Logical. Whether to show a legend for cluster outlines. If
#'   \code{NULL} (default), a legend is shown when multiple clusters exist.
#'
#' @return If \code{images} is provided along with \code{definition}, returns a character vector
#'   of slice coordinates (e.g., \code{c("z = 10.5", "z = -3.2")}) suitable for passing to
#'   \code{slices()}. If \code{layer} is specified or \code{images} is not provided, returns
#'   a deferred specification object (class \code{"cluster_slices_spec"}) that will be resolved
#'   during the ggbrain rendering pipeline.
#'
#' @details
#'   The function performs 3D connected component labeling on the thresholded/masked image
#'   to identify distinct clusters. For each cluster meeting the minimum size criterion,
#'   the center of mass is computed. Slices are then selected at the center of mass
#'   coordinates along the specified plane.
#'
#'   When \code{outline = TRUE}, a thin outline mask is also generated for the selected clusters.
#'   This outline can be automatically added as a \code{geom_outline()} layer during rendering so
#'   that clusters are highlighted on each slice. By default, distinct colors are assigned to each
#'   cluster using a categorical palette, but a single \code{outline_color} or a custom vector of
#'   colors can also be provided. The outline thickness can be controlled via
#'   \code{outline_size}.
#'
#'   The neighborhood connectivity parameter (\code{nn}) controls how voxels are considered
#'   connected:
#'   \itemize{
#'     \item \code{nn=1} (6-connectivity): Only voxels sharing a face are connected
#'     \item \code{nn=2} (18-connectivity): Voxels sharing a face or edge are connected
#'     \item \code{nn=3} (26-connectivity): Voxels sharing a face, edge, or corner are connected
#'   }
#'
#' @examples
#' \dontrun{
#'   # Using a definition string directly
#'   t1 <- system.file("extdata", "mni_template_2009c_2mm.nii.gz", package = "ggbrain")
#'   overlay <- system.file("extdata", "pe_ptfce_fwep_0.05_2mm.nii.gz", package = "ggbrain")
#'
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, overlay = overlay)) +
#'     slices(cluster_slices(
#'       images = c(overlay = overlay),
#'       definition = "overlay[abs(overlay) > 2.5]",
#'       nclusters = 5,
#'       min_clust_size = 50,
#'       plane = "axial"
#'     )) +
#'     geom_brain("underlay") +
#'     geom_brain(definition = "overlay[abs(overlay) > 2.5]")
#'
#'   # Or defer resolution until render time (images from ggbrain pipeline)
#'   gg_obj <- ggbrain() +
#'     images(c(underlay = t1, overlay = overlay)) +
#'     slices(cluster_slices(
#'       definition = "overlay[abs(overlay) > 2.5]",
#'       nclusters = 5
#'     )) +
#'     geom_brain("underlay") +
#'     geom_brain(definition = "overlay[abs(overlay) > 2.5]")
#' }
#'
#' @importFrom checkmate assert_count assert_choice assert_string assert_class
#' @export
cluster_slices <- function(images = NULL, layer = NULL, definition = NULL,
                           nclusters = 10, min_clust_size = 1, plane = "axial", nn = 3,
                           outline = FALSE, outline_color = NULL, outline_size = 1L,
                           outline_scale = NULL, outline_show_legend = NULL) {
  # Validate mutually exclusive arguments
  if (!is.null(layer) && !is.null(definition)) {
    stop("Specify either 'layer' or 'definition', not both.")
  }
  if (is.null(layer) && is.null(definition)) {
    stop("Must specify either 'layer' (name of a geom_brain layer) or 'definition' (a contrast string).")
  }

  # Validate other arguments
  checkmate::assert_count(nclusters, positive = TRUE)
  checkmate::assert_count(min_clust_size, positive = TRUE)
  plane <- tolower(plane)
  checkmate::assert_choice(plane, c("axial", "sagittal", "coronal", "x", "y", "z"))
  plane <- switch(plane, x = "sagittal", y = "coronal", z = "axial", plane)
  checkmate::assert_choice(nn, c(1L, 2L, 3L))
  checkmate::assert_logical(outline, len = 1L)
  checkmate::assert_integerish(outline_size, lower = 1L, len = 1L)
  checkmate::assert_character(outline_color, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_class(outline_scale, "Scale", null.ok = TRUE)
  checkmate::assert_logical(outline_show_legend, len = 1L, null.ok = TRUE)

  if (!is.null(outline_color) && length(outline_color) > 1L) {
    stop("outline_color must be a single value when using cluster_slices(outline = TRUE).")
  }
  if (!is.null(outline_scale) && !is.null(outline_color)) {
    stop("outline_color and outline_scale are mutually exclusive. Use outline_color for a fixed outline, or outline_scale with mapped outlines.")
  }

  # Determine if we can compute now or must defer
  can_compute_now <- !is.null(images) && !is.null(definition)

  if (can_compute_now) {
    # We have everything needed - compute slice locations now
    ret <- compute_cluster_slices(images, definition, nclusters, min_clust_size, plane, nn,
      outline = outline, outline_color = outline_color, outline_size = outline_size,
      outline_scale = outline_scale, outline_show_legend = outline_show_legend)
  } else {
    # Return a deferred specification to be resolved during render()
    ret <- structure(
      list(
        layer = layer,
        definition = definition,
        nclusters = nclusters,
        min_clust_size = min_clust_size,
        plane = plane,
        nn = nn,
        outline = outline,
        outline_color = outline_color,
        outline_size = outline_size,
        outline_scale = outline_scale,
        outline_show_legend = outline_show_legend
      ),
      class = "cluster_slices_spec"
    )
  }

  return(ret)
}


#' Internal function to compute cluster slice locations
#'
#' @param images A ggbrain_images object or named character vector of image paths
#' @param definition The definition/contrast string to apply
#' @param nclusters Maximum number of clusters
#' @param min_clust_size Minimum cluster size in voxels
#' @param plane The plane for slice selection
#' @param nn Neighborhood connectivity (1, 2, or 3)
#' @return A list with elements: coordinates (character vector of slice coords) and
#'   cluster_data (data.frame with cluster details). When outlines are requested, the list
#'   also includes \code{labeled_volume} (cluster ids restricted to the top clusters),
#'   \code{outline_palette} (colors for each cluster), \code{outline_scale} (ggplot2 scale, if provided),
#'   \code{outline_size} (integer), and \code{outline_show_legend} (logical).
#' @keywords internal
compute_cluster_slices <- function(images, definition, nclusters, min_clust_size, plane, nn,
  outline = FALSE, outline_color = NULL, outline_size = 1L, outline_scale = NULL, outline_show_legend = NULL) {
  # 1. Get the ggbrain_images object
  if (inherits(images, "character")) {
    img_obj <- ggbrain_images$new(images)
  } else if (inherits(images, "ggbrain_images")) {
    img_obj <- images$clone(deep = TRUE)
  } else {
    stop("images must be a character vector of file paths or a ggbrain_images object")
  }

  # 2. Apply the definition to get a 3D masked volume
  masked_vol <- apply_definition_3d(img_obj, definition)

  # Determine which image header should be used for voxel->world transforms
  # Prefer the base image referenced in the definition (text before any [subset]).
  definition_trim <- trimws(definition)
  base_img <- if (grepl("\\[", definition_trim)) {
    trimws(sub("\\s*\\[.*", "", definition_trim))
  } else {
    definition_trim
  }
  img_names <- img_obj$get_image_names()
  if (!base_img %in% img_names) {
    stop(glue::glue(
      "Image '{base_img}' not found in provided images. Available images: {paste(img_names, collapse = ', ')}"
    ))
  }

  # 3. Find 3D connected components and their properties
  cluster_info <- find_3d_clusters(masked_vol, min_size = min_clust_size, nn = nn, return_labels = outline)
  labeled_volume <- attr(cluster_info, "labeled_volume")

  if (nrow(cluster_info) == 0) {
    warning("No clusters found meeting the size criterion. Returning empty slice specification.")
    return(list(
      coordinates = character(0),
      cluster_data = data.frame(
        cluster_id = integer(0), size = integer(0), cluster_index = integer(0),
        com_i = numeric(0), com_j = numeric(0), com_k = numeric(0),
        com_x = numeric(0), com_y = numeric(0), com_z = numeric(0),
        slice_coord = character(0), outline_color = character(0), stringsAsFactors = FALSE
      ),
      labeled_volume = NULL,
      outline_palette = NULL,
      outline_size = outline_size
    ))
  }

  # 4. Select top clusters by size
  cluster_info <- cluster_info[order(-cluster_info$size), ]
  top_clusters <- head(cluster_info, nclusters)
  top_clusters$cluster_index <- seq_len(nrow(top_clusters))
  if (nrow(top_clusters) < nclusters) {
    warning(glue::glue(
      "Only {nrow(top_clusters)} cluster(s) found (requested {nclusters}). Consider thresholding or lowering min_clust_size."
    ))
  }

  # 5. Get the NIfTI header for coordinate transformation
  nii_head <- if (base_img %in% img_obj$get_image_names()) {
    img_obj$get_headers(img_names = base_img, drop = TRUE)
  } else {
    # Fallback: use the first available header, ensuring a single niftiHeader object
    hdr <- img_obj$get_headers(drop = TRUE)
    if (is.list(hdr)) hdr[[1L]] else hdr
  }

  # 6. Convert voxel coordinates to world coordinates
  # Center of mass in voxel space (1-indexed for R)
  com_voxels <- as.matrix(top_clusters[, c("com_i", "com_j", "com_k")])
  com_world <- RNifti::voxelToWorld(com_voxels, nii_head)
  if (is.null(dim(com_world))) {
    com_world <- matrix(com_world, ncol = 3, byrow = TRUE)
  }

  # Add world coordinates to cluster data
  top_clusters$com_x <- com_world[, 1]
  top_clusters$com_y <- com_world[, 2]
  top_clusters$com_z <- com_world[, 3]

  # 7. Extract the relevant coordinate for the requested plane
  axis <- switch(plane, axial = "z", sagittal = "x", coronal = "y")
  coord_idx <- switch(plane, sagittal = 1, coronal = 2, axial = 3)
  coords <- com_world[, coord_idx]

  # 8. Round coordinates for display and create slice strings
  coords <- round(coords, 1)
  slice_coords <- sprintf("%s = %s", axis, coords)

  # Add slice coordinates to cluster data

  top_clusters$slice_coord <- slice_coords

  # If requested, build a labeled volume for the selected clusters only
  if (!is.null(labeled_volume)) {
    vol_dims <- dim(labeled_volume)
    keep_ids <- top_clusters$cluster_id
    if (length(keep_ids) == 0L) {
      labeled_volume <- NULL
    } else {
      labeled_volume[!(labeled_volume %in% keep_ids)] <- NA_integer_
      id_map <- setNames(seq_along(keep_ids), keep_ids)
      labeled_volume <- id_map[as.character(labeled_volume)]
      labeled_volume <- array(as.integer(labeled_volume), dim = vol_dims)
    }
  }

  outline_palette <- NULL
  if (isTRUE(outline) && nrow(top_clusters) > 0L && is.null(outline_scale)) {
    n_pal <- nrow(top_clusters)
    if (is.null(outline_color)) {
      outline_palette <- grDevices::hcl.colors(n_pal, palette = "Dark 3")
    } else if (length(outline_color) == 1L) {
      outline_palette <- rep(outline_color, n_pal)
    }
    top_clusters$outline_color <- outline_palette[seq_len(nrow(top_clusters))]
  }

  # Remove duplicates from slice_coords for plotting (but keep full cluster data)
  unique_coords <- unique(slice_coords)

  return(list(
    coordinates = unique_coords,
    cluster_data = top_clusters,
    labeled_volume = labeled_volume,
    outline_palette = outline_palette,
    outline_size = outline_size,
    outline_scale = outline_scale,
    outline_show_legend = outline_show_legend
  ))
}


#' Apply a definition/contrast string to a 3D image volume
#'
#' @param img_obj A ggbrain_images object
#' @param definition A definition string, e.g., a simple image name or a filtered expression
#' @return A 3D array with the contrast applied (masked/thresholded volume)
#' @keywords internal
apply_definition_3d <- function(img_obj, definition) {
  checkmate::assert_class(img_obj, "ggbrain_images")
  checkmate::assert_string(definition)

  # Parse the definition to extract image name and any subsetting
  # Simple case: just an image name
  img_names <- img_obj$get_image_names()

  # Check if definition contains brackets (subsetting)
  has_brackets <- grepl("\\[", definition)

  if (!has_brackets) {
    # Simple image reference
    if (!definition %in% img_names) {
      stop(glue::glue("Image '{definition}' not found. Available images: {paste(img_names, collapse=', ')}"))
    }
    vol <- img_obj$get_images(definition, drop = TRUE)
    # Create binary mask for non-zero values
    mask <- abs(as.array(vol)) > img_obj$zero_tol
    return(mask)
  }

  # Complex definition with subsetting - need to parse and evaluate
  # Extract the base image name (before the bracket)
  base_img <- sub("\\s*\\[.*", "", definition)
  base_img <- trimws(base_img)

  if (!base_img %in% img_names) {
    stop(glue::glue("Image '{base_img}' not found. Available images: {paste(img_names, collapse=', ')}"))
  }

  # Get the full 3D volume
  vol <- as.array(img_obj$get_images(base_img, drop = TRUE))

  # Extract the condition from brackets using perl regex for proper matching
  bracket_match <- regmatches(definition, regexpr("\\[([^\\]]+)\\]", definition, perl = TRUE))
  if (length(bracket_match) == 0 || bracket_match == "") {
    stop("Could not parse bracket expression in definition")
  }

  condition <- gsub("^\\[|\\]$", "", bracket_match)

  # Build evaluation environment with all referenced images (and value for backward compatibility)
  expr <- parse(text = condition)
  vars <- all.names(expr)
  ref_imgs <- unique(intersect(vars, img_names))
  eval_env <- list(value = vol)
  eval_env[[base_img]] <- vol
  for (nm in ref_imgs) {
    arr <- as.array(img_obj$get_images(nm, drop = TRUE))
    eval_env[[nm]] <- arr
  }

  mask <- tryCatch(
    {
      eval(expr, envir = eval_env)
    },
    error = function(e) {
      stop(glue::glue("Error evaluating condition '{condition}': {e$message}"))
    }
  )

  # Ensure result is an array of the correct shape
  if (is.null(dim(mask)) && length(mask) == 1L) {
    stop(glue::glue("Condition '{condition}' did not return an array; it must evaluate to a per-voxel logical/ numeric volume."))
  }
  if (!identical(dim(mask), dim(vol))) {
    stop(glue::glue("Condition '{condition}' returned an array with dimensions {paste(dim(mask), collapse='x')} that do not match the base image ({paste(dim(vol), collapse='x')})."))
  }

  # Ensure result is logical
  if (!is.logical(mask)) {
    mask <- abs(mask) > img_obj$zero_tol
  }

  return(mask)
}


#' Find 3D connected components in a binary volume
#'
#' @param mask A 3D logical array (TRUE for voxels of interest)
#' @param min_size Minimum cluster size to retain
#' @param nn Neighborhood connectivity (1=6-conn, 2=18-conn, 3=26-conn)
#' @param return_labels If TRUE, attaches the labeled volume (after size filtering) as an attribute
#'   named \code{labeled_volume}.
#' @return A data.frame with columns: cluster_id, size, com_i, com_j, com_k
#' @keywords internal
find_3d_clusters <- function(mask, min_size = 1, nn = 3, return_labels = FALSE) {
  checkmate::assert_array(mask)
  checkmate::assert_logical(as.vector(mask))

  dims <- dim(mask)
  if (length(dims) != 3) {
    stop("mask must be a 3D array")
  }

  # Use flood-fill based connected component labeling
  labeled <- label_3d_components(mask, nn = nn)

  # Get unique cluster labels (excluding 0 = background)
  cluster_ids <- sort(unique(as.vector(labeled)))
  cluster_ids <- cluster_ids[cluster_ids > 0]

  if (length(cluster_ids) == 0) {
    cluster_df <- data.frame(
      cluster_id = integer(0),
      size = integer(0),
      com_i = numeric(0),
      com_j = numeric(0),
      com_k = numeric(0)
    )
    if (isTRUE(return_labels)) attr(cluster_df, "labeled_volume") <- labeled
    return(cluster_df)
  }

  # Compute cluster statistics
  cluster_stats <- lapply(cluster_ids, function(cid) {
    voxels <- which(labeled == cid, arr.ind = TRUE)
    size <- nrow(voxels)

    if (size < min_size) {
      return(NULL)
    }

    # Center of mass (mean of voxel indices)
    com_i <- mean(voxels[, 1])
    com_j <- mean(voxels[, 2])
    com_k <- mean(voxels[, 3])

    data.frame(
      cluster_id = cid,
      size = size,
      com_i = com_i,
      com_j = com_j,
      com_k = com_k
    )
  })

  cluster_df <- do.call(rbind, cluster_stats)

  if (is.null(cluster_df) || nrow(cluster_df) == 0) {
    cluster_df <- data.frame(
      cluster_id = integer(0),
      size = integer(0),
      com_i = numeric(0),
      com_j = numeric(0),
      com_k = numeric(0)
    )
    if (isTRUE(return_labels)) attr(cluster_df, "labeled_volume") <- labeled
    return(cluster_df)
  }

  # Remove any clusters below the minimum size from the labeled volume if requested
  if (isTRUE(return_labels)) {
    keep_ids <- cluster_df$cluster_id
    labeled[!(labeled %in% keep_ids)] <- 0L
    attr(cluster_df, "labeled_volume") <- labeled
  }

  return(cluster_df)
}


#' 3D connected component labeling using two-pass algorithm with union-find
#'
#' @param mask A 3D logical array
#' @param nn Neighborhood connectivity (1=6-conn, 2=18-conn, 3=26-conn)
#' @return A 3D integer array with cluster labels (0 = background)
#' @keywords internal
label_3d_components <- function(mask, nn = 3) {
  dims <- dim(mask)
  ni <- dims[1]
  nj <- dims[2]
  nk <- dims[3]

  # Get neighbor offsets - only use "backward" neighbors for two-pass algorithm
  # (neighbors with smaller indices that have already been processed)
  offsets <- get_backward_neighbor_offsets(nn)

  # Initialize label array and union-find structure
  labels <- array(0L, dim = dims)
  next_label <- 1L

  # Union-find data structure (will grow as needed)
  # parent[i] gives the parent of label i; root has parent[i] == i
  parent <- integer(0)

  # Helper functions for union-find
  find_root <- function(x) {
    while (parent[x] != x) {
      parent[x] <<- parent[parent[x]]  # path compression
      x <- parent[x]
    }
    x
  }

  union_labels <- function(a, b) {
    root_a <- find_root(a)
    root_b <- find_root(b)
    if (root_a != root_b) {
      # Union by making smaller root the parent
      if (root_a < root_b) {
        parent[root_b] <<- root_a
      } else {
        parent[root_a] <<- root_b
      }
    }
  }

  # First pass: assign provisional labels and record equivalences
  for (k in seq_len(nk)) {
    for (j in seq_len(nj)) {
      for (i in seq_len(ni)) {
        if (!mask[i, j, k]) next

        # Check backward neighbors for existing labels
        neighbor_labels <- integer(0)
        for (oi in seq_len(nrow(offsets))) {
          ni_new <- i + offsets[oi, 1]
          nj_new <- j + offsets[oi, 2]
          nk_new <- k + offsets[oi, 3]

          if (ni_new >= 1 && ni_new <= ni &&
              nj_new >= 1 && nj_new <= nj &&
              nk_new >= 1 && nk_new <= nk &&
              labels[ni_new, nj_new, nk_new] > 0L) {
            neighbor_labels <- c(neighbor_labels, labels[ni_new, nj_new, nk_new])
          }
        }

        if (length(neighbor_labels) == 0) {
          # No labeled neighbors - create new label
          labels[i, j, k] <- next_label
          parent <- c(parent, next_label)  # parent[next_label] = next_label (root)
          next_label <- next_label + 1L
        } else {
          # Use minimum neighbor label
          min_label <- min(neighbor_labels)
          labels[i, j, k] <- min_label

          # Union all neighbor labels
          for (nl in unique(neighbor_labels)) {
            if (nl != min_label) {
              union_labels(min_label, nl)
            }
          }
        }
      }
    }
  }

  # Second pass: replace each label with its root
  if (length(parent) > 0) {
    # Create mapping from old labels to new consecutive labels
    roots <- sapply(seq_along(parent), find_root)
    unique_roots <- sort(unique(roots))
    label_map <- integer(length(parent))
    label_map[unique_roots] <- seq_along(unique_roots)

    # Apply mapping to all labeled voxels
    labeled_idx <- which(labels > 0)
    labels[labeled_idx] <- label_map[roots[labels[labeled_idx]]]
  }

  return(labels)
}


#' Get backward neighbor offsets for two-pass connected component labeling
#'
#' @param nn Connectivity level (1, 2, or 3)
#' @return A matrix of backward neighbor offsets (n x 3)
#' @keywords internal
get_backward_neighbor_offsets <- function(nn) {
  # For two-pass algorithm, we only need neighbors that come before current voxel
  # in raster scan order (i varies fastest, then j, then k)

  # 6-connectivity: only 3 backward face neighbors
  face_offsets <- rbind(
    c(-1, 0, 0),   # i-1
    c(0, -1, 0),   # j-1
    c(0, 0, -1)    # k-1
  )

  if (nn == 1) {
    return(face_offsets)
  }

  # 18-connectivity: add backward edge neighbors
 edge_offsets <- rbind(
    c(-1, -1, 0), c(-1, 1, 0),   # edges in k plane with i-1
    c(-1, 0, -1), c(-1, 0, 1),   # edges in j plane with i-1 (only k-1 is backward for second)
    c(0, -1, -1), c(0, -1, 1),   # edges in i plane with j-1 (only k-1 is backward for second)
    c(1, -1, 0),                 # edge with j-1, i+1 (j-1 makes it backward)
    c(1, 0, -1),                 # edge with k-1, i+1
    c(0, 1, -1)                  # edge with k-1, j+1
  )

  # Filter to only truly backward neighbors
  edge_offsets_backward <- rbind(
    c(-1, -1, 0),
    c(-1, 0, -1),
    c(0, -1, -1),
    c(-1, 1, 0),   # i-1 makes it backward
    c(-1, 0, 1),   # i-1 makes it backward (but we need k to have been visited... skip)
    c(1, -1, 0),   # j-1 makes it backward
    c(1, 0, -1),   # k-1 makes it backward
    c(0, 1, -1)    # k-1 makes it backward
  )

  if (nn == 2) {
    return(rbind(face_offsets, edge_offsets_backward))
  }

  # 26-connectivity: add backward corner neighbors
  corner_offsets_backward <- rbind(
    c(-1, -1, -1),
    c(-1, -1, 1),  # i-1, j-1 backward
    c(-1, 1, -1),  # i-1, k-1 backward
    c(-1, 1, 1),   # i-1 backward
    c(1, -1, -1),  # j-1, k-1 backward
    c(1, -1, 1),   # j-1 backward
    c(1, 1, -1)    # k-1 backward
  )

  return(rbind(face_offsets, edge_offsets_backward, corner_offsets_backward))
}


#' Get neighbor offsets for 3D connectivity (all directions)
#'
#' @param nn Connectivity level (1, 2, or 3)
#' @return A matrix of neighbor offsets (n x 3)
#' @keywords internal
get_neighbor_offsets <- function(nn) {
  # 6-connectivity: faces only
  face_offsets <- rbind(
    c(-1, 0, 0), c(1, 0, 0),
    c(0, -1, 0), c(0, 1, 0),
    c(0, 0, -1), c(0, 0, 1)
  )

  if (nn == 1) {
    return(face_offsets)
  }

  # 18-connectivity: faces + edges
  edge_offsets <- rbind(
    c(-1, -1, 0), c(-1, 1, 0), c(1, -1, 0), c(1, 1, 0),
    c(-1, 0, -1), c(-1, 0, 1), c(1, 0, -1), c(1, 0, 1),
    c(0, -1, -1), c(0, -1, 1), c(0, 1, -1), c(0, 1, 1)
  )

  if (nn == 2) {
    return(rbind(face_offsets, edge_offsets))
  }

  # 26-connectivity: faces + edges + corners
  corner_offsets <- rbind(
    c(-1, -1, -1), c(-1, -1, 1), c(-1, 1, -1), c(-1, 1, 1),
    c(1, -1, -1), c(1, -1, 1), c(1, 1, -1), c(1, 1, 1)
  )

  return(rbind(face_offsets, edge_offsets, corner_offsets))
}


#' Check if an object is a cluster_slices_spec
#'
#' @param x Object to check
#' @return TRUE if x is a cluster_slices_spec, FALSE otherwise
#' @keywords internal
is_cluster_slices_spec <- function(x) {
  inherits(x, "cluster_slices_spec")
}


#' Resolve a deferred cluster_slices_spec
#'
#' @param spec A cluster_slices_spec object
#' @param images A ggbrain_images object
#' @param layers A list of ggbrain_layer objects (optional, for layer reference)
#' @return A list with elements: coordinates (character vector) and cluster_data (data.frame)
#' @keywords internal
resolve_cluster_slices <- function(spec, images, layers = NULL) {
  checkmate::assert_class(spec, "cluster_slices_spec")
  checkmate::assert_class(images, "ggbrain_images")

  # Determine the definition to use
  if (!is.null(spec$layer)) {
    # Look up definition from layers
    if (is.null(layers)) {
      stop("Cannot resolve layer reference without layers provided")
    }

    layer_names <- sapply(layers, function(l) l$name)
    layer_idx <- which(layer_names == spec$layer)

    if (length(layer_idx) == 0) {
      stop(glue::glue(
        "Layer '{spec$layer}' not found. Available layers: {paste(layer_names, collapse=', ')}"
      ))
    }

    definition <- layers[[layer_idx[1]]]$definition
  } else {
    definition <- spec$definition
  }

  # Now compute the cluster slices (returns list with coordinates and cluster_data)
  # For clusterized layers, always request a labeled volume (outline mask) even if outline is FALSE.
  outline_flag <- if (identical(spec$action, "clusterized")) TRUE else isTRUE(spec$outline)
  compute_cluster_slices(
    images = images,
    definition = definition,
    nclusters = spec$nclusters,
    min_clust_size = spec$min_clust_size,
    plane = spec$plane,
    nn = spec$nn,
    outline = outline_flag,
    outline_color = spec$outline_color,
    outline_size = spec$outline_size,
    outline_scale = spec$outline_scale,
    outline_show_legend = spec$outline_show_legend
  )
}
