# internal, single-level list flattener to replace rlang::flatten
# drops NULLs, preserves order, and wraps atomic elements in a list
flatten_list_once <- function(x) {
  stopifnot(is.list(x))

  out <- list()
  for (i in seq_along(x)) {
    elt <- x[[i]]
    if (is.null(elt)) {
      next
    } else if (is.list(elt)) {
      out <- c(out, elt)
    } else {
      out <- c(out, list(elt))
    }
  }

  out
}

# Choose a legible text color based on background luminance. Defaults favor high contrast
# for typical dark/light gray backgrounds used in ggbrain.
infer_text_color_from_bg <- function(bg_color, light = "gray90", dark = "gray10", threshold = 0.5) {
  if (is.null(bg_color) || length(bg_color) == 0L) return(dark)

  luminance <- tryCatch({
    rgb <- grDevices::col2rgb(bg_color)
    rgb <- rgb / 255
    rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    vals <- as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
    if (length(vals) > 1L) vals[1L] else vals
  }, error = function(e) NA_real_)

  if (is.na(luminance)) return(dark)
  if (luminance > threshold) dark else light
}
