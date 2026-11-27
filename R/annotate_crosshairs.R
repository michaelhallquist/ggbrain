#' Annotate crosshairs at world coordinates or cluster peaks
#'
#' @param xyz A numeric matrix or data.frame with columns \code{x}, \code{y}, and \code{z}
#'   giving world-space coordinates (in mm) at which to draw crosshairs.
#' @param color Line color for the crosshairs. Default: "white".
#' @param linewidth Line width passed to `ggplot2::annotate`. Default: 0.4.
#' @param alpha Line alpha. Default: 0.7.
#' @param linetype Line type (e.g., "dashed"). Default: "dashed".
#' @param tol Tolerance (in mm) for matching a coordinate to a displayed slice along its
#'   slicing axis when using `xyz`. Default: 1.
#' @param ... Additional arguments passed to `ggplot2::annotate` for the crosshair segments.
#' @param at Set to `"cluster_peaks"` to mark peaks from
#'   `slices(cluster_slices(..., slice_at = "peak"))`. Supply either `xyz` or `at`.
#'
#' @details
#' Crosshairs are drawn on slices whose slicing axis (e.g., z for axial) falls within
#' `tol` mm of the requested coordinate. If the coordinate on the slicing axis is `NA`,
#' the crosshair is shown on all slices along that axis where the remaining two axes match.
#' For example, `x=0, y=0, z=NA` will show a crosshair on every axial slice at x/y=0 (within
#' the nearest in-plane voxel). Coordinates are provided in world space to match the
#' `slices()` API.
#'
#' With `at = "cluster_peaks"`, each peak is marked on every displayed slice
#' that passes through its voxel. The slice match uses the exact voxel index.
#' If several peaks lie on a slice, each gets a crosshair. When no peaks are
#' selected, this annotation draws nothing.
#'
#' @examples
#' \dontrun{
#' # Show crosshairs on any axial slice at x/y = 0
#' ggbrain() +
#'   images(c(underlay = underlay_2mm)) +
#'   slices(c("z = 20", "z = 40")) +
#'   geom_brain("underlay") +
#'   annotate_crosshairs(data.frame(x = 0, y = 0, z = NA), linewidth = 0.6)
#' }
#'
#' @return A `ggb` object with action "add_annotations"
#' @export
annotate_crosshairs <- function(xyz = NULL, color = "white", linewidth = 0.4, alpha = 0.7,
                                linetype = "dashed", tol = 1, ..., at = NULL) {
  checkmate::assert_choice(at, "cluster_peaks", null.ok = TRUE)
  if (!is.null(at) && !is.null(xyz)) {
    stop("Arguments 'xyz' and 'at' are mutually exclusive.")
  }
  if (is.null(xyz) && is.null(at)) return(ggb$new())

  if (!is.null(xyz)) {
    if (is.matrix(xyz)) xyz <- as.data.frame(xyz)
    checkmate::assert_data_frame(xyz)
    checkmate::assert_subset(c("x", "y", "z"), names(xyz))
    xyz <- xyz[, c("x", "y", "z")]
    xyz[] <- lapply(xyz, as.numeric)
  }

  params <- list(
    xyz = xyz,
    at = at,
    color = color,
    linewidth = linewidth,
    alpha = alpha,
    linetype = linetype,
    tol = tol,
    extra = list(...)
  )

  ggb$new(annotations = list(list(crosshair_params = params)), action = "add_annotations")
}

# Internal helper: build crosshair annotations for a given slice
crosshair_annotations_for_slice <- function(slices_obj, slice_index, xyz, color, linewidth, alpha,
                                            linetype, tol = 1, extra = NULL, ijk = NULL) {
  grid <- slices_obj$slice_grid[[slice_index]]
  positions <- ijk %||% xyz
  if (is.null(grid) || NROW(positions) == 0L) return(NULL)

  # Both coordinate systems store the slicing and in-plane axes in the same order.
  axes <- switch(
    grid$plane,
    sagittal = c(1L, 2L, 3L),
    coronal = c(2L, 1L, 3L),
    axial = c(3L, 1L, 2L),
    stop("Unknown plane: ", grid$plane)
  )
  axis_vals <- positions[[axes[1L]]]
  if (is.null(ijk)) {
    axis_match <- is.na(axis_vals) | abs(axis_vals - grid$slice_coord_world) <= tol
    dim1 <- grid$dim1_world
    dim2 <- grid$dim2_world
  } else {
    axis_match <- axis_vals == grid$slice_number
    dim1 <- grid$dim1_vox
    dim2 <- grid$dim2_vox
  }
  keep <- which(axis_match)
  if (!length(keep)) return(NULL)

  nearest_index <- function(vec, target) {
    valid <- which(!is.na(vec))
    if (!length(valid)) return(NA_real_)
    vals <- vec[valid]
    valid[which.min(abs(vals - target))]
  }

  ann_list <- lapply(keep, function(idx) {
    dim1_target <- positions[[axes[2L]]][idx]
    dim2_target <- positions[[axes[3L]]][idx]

    if (is.na(dim1_target) || is.na(dim2_target)) return(NULL) # need both in-plane coords

    x_idx <- nearest_index(dim1, dim1_target)
    y_idx <- nearest_index(dim2, dim2_target)
    if (is.na(x_idx) || is.na(y_idx)) return(NULL)

    seg_args <- c(
      list(color = color, linewidth = linewidth, alpha = alpha, linetype = linetype, allow_oob = TRUE),
      extra
    )

    vert <- tibble::tibble(
      geom = "segment",
      x = x_idx,
      xend = x_idx,
      y = 1,
      yend = grid$dim2_len
    )
    horiz <- tibble::tibble(
      geom = "segment",
      x = 1,
      xend = grid$dim1_len,
      y = y_idx,
      yend = y_idx
    )

    vert <- dplyr::bind_cols(vert, seg_args)
    horiz <- dplyr::bind_cols(horiz, seg_args)
    dplyr::bind_rows(vert, horiz)
  })

  ret <- dplyr::bind_rows(ann_list)
  if (nrow(ret) == 0L) return(NULL)
  ret
}
