#' Annotate crosshairs at user-specified world (xyz) coordinates
#'
#' @param xyz A numeric matrix or data.frame with columns \code{x}, \code{y}, and \code{z}
#'   giving world-space coordinates (in mm) at which to draw crosshairs.
#' @param color Line color for the crosshairs. Default: "white".
#' @param linewidth Line width passed to `ggplot2::annotate`. Default: 0.4.
#' @param alpha Line alpha. Default: 0.7.
#' @param linetype Line type (e.g., "dashed"). Default: "dashed".
#' @param tol Tolerance (in mm) for matching a coordinate to a displayed slice along its
#'   slicing axis. Default: 1.
#' @param ... Additional arguments passed to `ggplot2::annotate` for the crosshair segments.
#'
#' @details
#' Crosshairs are drawn on slices whose slicing axis (e.g., z for axial) falls within
#' `tol` mm of the requested coordinate. If the coordinate on the slicing axis is `NA`,
#' the crosshair is shown on all slices along that axis where the remaining two axes match.
#' For example, `x=0, y=0, z=NA` will show a crosshair on every axial slice at x/y=0 (within
#' the nearest in-plane voxel). Coordinates are provided in world space to match the
#' `slices()` API.
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
annotate_crosshairs <- function(xyz, color = "white", linewidth = 0.4, alpha = 0.7, linetype = "dashed", tol = 1, ...) {
  if (is.null(xyz)) return(ggb$new()) # nothing to do

  if (is.matrix(xyz)) xyz <- as.data.frame(xyz)
  checkmate::assert_data_frame(xyz)
  checkmate::assert_subset(c("x", "y", "z"), names(xyz))
  xyz <- xyz[, c("x", "y", "z")]
  xyz$x <- as.numeric(xyz$x); xyz$y <- as.numeric(xyz$y); xyz$z <- as.numeric(xyz$z)

  params <- list(
    xyz = xyz,
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
crosshair_annotations_for_slice <- function(slices_obj, slice_index, xyz, color, linewidth, alpha, linetype, tol = 1, extra = NULL) {
  grid <- slices_obj$slice_grid[[slice_index]]
  if (is.null(grid)) return(NULL)

  plane <- grid$plane
  slice_coord <- grid$slice_coord_world

  # decide which coordinates fall on this slice (compare along slicing axis)
  axis_vals <- switch(
    plane,
    sagittal = xyz$x,
    coronal = xyz$y,
    axial = xyz$z,
    stop("Unknown plane: ", plane)
  )
  axis_match <- ifelse(is.na(axis_vals), TRUE, abs(axis_vals - slice_coord) <= tol)
  keep <- which(axis_match)
  if (!length(keep)) return(NULL)

  nearest_index <- function(vec, target) {
    valid <- which(!is.na(vec))
    if (!length(valid)) return(NA_real_)
    vals <- vec[valid]
    valid[which.min(abs(vals - target))]
  }

  ann_list <- lapply(keep, function(idx) {
    target <- xyz[idx, ]
    dim1_target <- switch(plane,
      sagittal = target$y,
      coronal = target$x,
      axial = target$x
    )
    dim2_target <- switch(plane,
      sagittal = target$z,
      coronal = target$z,
      axial = target$y
    )

    if (is.na(dim1_target) || is.na(dim2_target)) return(NULL) # need both in-plane coords

    x_idx <- nearest_index(grid$dim1_world, dim1_target)
    y_idx <- nearest_index(grid$dim2_world, dim2_target)
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
