#' Annotate orientation labels (L/R/A/P/S/I) on each panel using NIfTI header info
#'
#' @param size Text size passed to ggplot2::annotate
#' @param color Text color passed to ggplot2::annotate. If NULL, defaults to the panel text color.
#' @param offset Offset to nudge labels away from the image edge. Can be scalar or length 4 (left, right, bottom, top).
#'   Numeric values are in data units; percentage strings (e.g., "10%") are interpreted relative to the panel width (left/right)
#'   or height (bottom/top).
#' @param show Character vector of labels to display. Valid values are any of \code{c("A","P","I","S","L","R")}
#'   or \code{"All"} (default) to show all labels.
#' @param ... Additional arguments passed to ggplot2::annotate (e.g., fontface)
#'
#' @details
#' Uses RNifti::orientation on the first image in the ggbrain object (cached in the slices object) to infer axis labels and places
#' them along the edges of each panel. If orientation cannot be determined, no annotations are added.
#' 
#' @return a `ggb` object with action "add_annotations"
#' @export
annotate_orientation <- function(size = 3, color = NULL, offset = 0, show = "all", ...) {
  params <- list(size = size, color = color, offset = offset, show = show, extra = list(...))
  ggb$new(annotations = list(list(orientation_params = params)), action = "add_annotations")
}

# internal helper: build orientation annotations as data.frame rows for a given panel
orientation_annotations_for_slice <- function(panel_index, slices_obj, size = 3, color = NULL, offset = 0,
                                              show = "all", bg_color = NULL, ...) {
  ori <- slices_obj$get_orientation_labels()
  if (is.null(ori)) return(NULL)

  plane <- slices_obj$coord_input[[panel_index]]
  if (length(plane) == 1 && is.character(plane)) plane <- tolower(substr(plane, 1, 1))

  slice_data <- slices_obj$slice_data[[panel_index]]
  dims <- slice_data[[1]]
  x_range <- range(dims$dim1, na.rm = TRUE)
  y_range <- range(dims$dim2, na.rm = TRUE)

  map <- switch(
    plane,
    x = c("y", "z"), i = c("y", "z"),
    y = c("x", "z"), j = c("x", "z"),
    z = c("x", "y"), k = c("x", "y"),
    c(NA, NA)
  )
  if (any(is.na(map))) return(NULL)

  x_labels <- ori[[map[1]]]; y_labels <- ori[[map[2]]]
  if (any(is.na(c(x_labels, y_labels)))) return(NULL)

  offs <- rep(offset, length.out = 4)
  parse_off <- function(val, len) {
    if (is.character(val) && grepl("^\\s*-?[0-9.]+%\\s*$", val)) {
      pct <- as.numeric(sub("%", "", val)) / 100
      return(pct * len)
    }
    checkmate::assert_number(val, na.ok = FALSE)
    val
  }
  offs <- vapply(seq_along(offs), function(i) {
    len <- if (i <= 2) diff(x_range) else diff(y_range)
    parse_off(offs[[i]], len)
  }, numeric(1))
  if (is.null(color) || length(color) == 0L) color <- infer_text_color_from_bg(bg_color)

  ann_df <- tibble::tibble(
    label = c(x_labels[1], x_labels[2], y_labels[1], y_labels[2]),
    x = c(x_range[1] - offs[1], x_range[2] + offs[2], mean(x_range), mean(x_range)),
    y = c(mean(y_range), mean(y_range), y_range[1] - offs[3], y_range[2] + offs[4]),
    hjust = c(1, 0, 0.5, 0.5),
    vjust = c(0.5, 0.5, 1, 0),
    geom = "text",
    size = size,
    color = color,
    allow_oob = TRUE,
    ...
  )

  show <- toupper(show)
  valid <- c("A", "P", "I", "S", "L", "R")
  if (length(show) > 0L && !any(show == "ALL")) {
    bad <- setdiff(show, valid)
    if (length(bad)) stop("Invalid orientation labels in `show`: ", paste(bad, collapse = ", "))
    ann_df <- ann_df[ann_df$label %in% show, , drop = FALSE]
  }

  ann_df
}
