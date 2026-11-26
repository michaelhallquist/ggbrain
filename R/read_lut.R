# Internal helpers for parsing color lookup table (LUT) lines
strip_lut_comment <- function(line) {
  # Preserve any '#' characters inside double-quoted labels (e.g., ITK-SNAP)
  if (!grepl("\"", line, fixed = TRUE)) {
    return(sub("\\s+#.*$", "", line))
  }
  # Strip comments only after the final closing quote
  q_pos <- gregexpr("\"", line, fixed = TRUE)[[1L]]
  last_q <- q_pos[length(q_pos)]
  tail_part <- substr(line, last_q + 1L, nchar(line))
  if (grepl("#", tail_part, fixed = TRUE)) {
    tail_part <- sub("\\s*#.*$", "", tail_part)
    return(paste0(substr(line, 1L, last_q), tail_part))
  }
  line
}

tokenize_lut_line <- function(line) {
  cleaned <- trimws(strip_lut_comment(line))
  if (!nzchar(cleaned)) return(character(0))
  scan(
    text = cleaned,
    what = character(),
    quiet = TRUE,
    quote = "\"",
    comment.char = "",
    allowEscapes = FALSE
  )
}

is_numeric_token <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

is_integer_token <- function(x) {
  num <- suppressWarnings(as.numeric(x))
  !is.na(num) && is.finite(num) && abs(num - round(num)) < 1e-8
}

detect_lut_format <- function(tokens_list, raw_lines) {
  if (any(grepl("ITK-SnAP", raw_lines, ignore.case = TRUE))) {
    return("itksnap")
  }

  tok <- tokens_list[[1L]]
  n_tok <- length(tok)

  # ITK-SNAP: IDX R G B A VIS MSH "Label" (8+ tokens, first 7 numeric)
  if (n_tok >= 8L && all(is_numeric_token(tok[1:7])) && is_integer_token(tok[1L])) {
    return("itksnap")
  }

  # FreeSurfer: index label R G B [A] (5 or 6 tokens, col 1 int, col 2 label, cols 3-5 numeric)
  if (n_tok %in% c(5L, 6L) &&
      is_integer_token(tok[1L]) &&
      !is_numeric_token(tok[2L]) &&
      all(is_numeric_token(tok[3:5])) &&
      (n_tok == 5L || is_numeric_token(tok[6L]))) {
    return("freesurfer")
  }

  # FSL / FSLeyes: index R G B label... (5+ tokens, cols 1-4 numeric, col 5+ label)
  if (n_tok >= 5L &&
      is_integer_token(tok[1L]) &&
      all(is_numeric_token(tok[2:4])) &&
      !is_numeric_token(tok[5L])) {
    return("fsl")
  }

  stop(
    "Could not automatically detect LUT format from the first entry: '",
    paste(tok, collapse = " "),
    "'. Specify `format` explicitly as one of 'freesurfer', 'itksnap', or 'fsl'."
  )
}

#' Read a neuroimaging color lookup table (LUT)
#'
#' Parses common neuroimaging atlas color lookup tables into a standardized
#' tibble containing integer region values, text labels, RGB channels, and hex
#' colors. The returned table can be passed directly to \code{\link{images}(..., labels = lut)}
#' and \code{\link{scale_fill_lut}()}.
#'
#' @param file Path to a text color lookup table file.
#' @param format Character string specifying the LUT format. One of:
#'   \itemize{
#'     \item \code{"auto"} (default): Detect format automatically from the header and first entry.
#'     \item \code{"freesurfer"}: Whitespace-delimited \code{index label R G B [A]} (e.g.,
#'       \code{FreeSurferColorLUT.txt}, Yeo, or Schaefer LUTs).
#'     \item \code{"itksnap"}: ITK-SNAP label description format
#'       \code{IDX R G B A VIS MSH "Label"}.
#'     \item \code{"fsl"}: FSL/FSLeyes label LUT format \code{index R G B label}
#'       (with RGB values in \code{[0, 1]} or \code{[0, 255]} and optional multi-word labels).
#'   }
#' @param drop_zero Logical; if \code{TRUE} (default), remove entries with \code{value == 0}
#'   (typically background/unknown labels such as \code{NONE}, \code{Unknown}, or \code{Clear Label}),
#'   since \code{ggbrain} treats voxel value 0 as transparent background.
#'
#' @return A \code{\link[tibble]{tibble}} with columns:
#'   \describe{
#'     \item{\code{value}}{Integer ROI/parcel code matching values in the NIfTI atlas.}
#'     \item{\code{label}}{Character region or network name.}
#'     \item{\code{r}, \code{g}, \code{b}}{Integer RGB channel values in \code{0..255}.}
#'     \item{\code{color}}{Hexadecimal RGB color string (e.g., \code{"#781286"}).}
#'   }
#'
#' @examples
#' lut_file <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")
#' yeo_lut <- read_lut(lut_file)
#' yeo_lut
#'
#' @seealso \code{\link{scale_fill_lut}()}, \code{\link{images}()}
#' @importFrom checkmate assert_file_exists assert_flag
#' @importFrom tibble tibble
#' @export
read_lut <- function(file, format = c("auto", "freesurfer", "itksnap", "fsl"), drop_zero = TRUE) {
  checkmate::assert_file_exists(file, access = "r")
  format <- match.arg(format)
  checkmate::assert_flag(drop_zero)

  raw_lines <- readLines(file, warn = FALSE)
  if (length(raw_lines) > 0L) {
    raw_lines[1L] <- sub("^\ufeff", "", raw_lines[1L])
  }

  trimmed <- trimws(raw_lines)
  keep_idx <- which(nzchar(trimmed) & !grepl("^(#|//)", trimmed))
  if (length(keep_idx) == 0L) {
    stop("No LUT entries found in file: ", file)
  }

  tokens_list <- lapply(raw_lines[keep_idx], tokenize_lut_line)
  non_empty <- lengths(tokens_list) > 0L
  keep_idx <- keep_idx[non_empty]
  tokens_list <- tokens_list[non_empty]

  if (length(tokens_list) == 0L) {
    stop("No LUT entries found in file: ", file)
  }

  resolved_format <- if (identical(format, "auto")) {
    detect_lut_format(tokens_list, raw_lines)
  } else {
    format
  }

  n_entries <- length(tokens_list)
  values <- integer(n_entries)
  labels <- character(n_entries)
  r_vals <- numeric(n_entries)
  g_vals <- numeric(n_entries)
  b_vals <- numeric(n_entries)

  for (i in seq_len(n_entries)) {
    tok <- tokens_list[[i]]
    line_num <- keep_idx[i]

    if (identical(resolved_format, "freesurfer")) {
      if (!length(tok) %in% c(5L, 6L)) {
        stop(sprintf(
          "Invalid FreeSurfer LUT entry on line %d: expected 5 or 6 fields, found %d.",
          line_num, length(tok)
        ))
      }
      val_tok <- tok[1L]
      lab_tok <- tok[2L]
      rgb_tok <- tok[3:5]
    } else if (identical(resolved_format, "itksnap")) {
      if (length(tok) < 8L) {
        stop(sprintf(
          "Invalid ITK-SNAP LUT entry on line %d: expected at least 8 fields, found %d.",
          line_num, length(tok)
        ))
      }
      val_tok <- tok[1L]
      rgb_tok <- tok[2:4]
      lab_tok <- paste(tok[8:length(tok)], collapse = " ")
    } else if (identical(resolved_format, "fsl")) {
      if (length(tok) < 5L) {
        stop(sprintf(
          "Invalid FSL LUT entry on line %d: expected at least 5 fields, found %d.",
          line_num, length(tok)
        ))
      }
      val_tok <- tok[1L]
      rgb_tok <- tok[2:4]
      lab_tok <- paste(tok[5:length(tok)], collapse = " ")
    }

    if (!is_integer_token(val_tok)) {
      stop(sprintf("Invalid integer ROI index '%s' on line %d.", val_tok, line_num))
    }
    rgb_num <- suppressWarnings(as.numeric(rgb_tok))
    if (any(is.na(rgb_num)) || any(!is.finite(rgb_num))) {
      stop(sprintf("Invalid RGB values '%s' on line %d.", paste(rgb_tok, collapse = " "), line_num))
    }

    values[i] <- as.integer(round(as.numeric(val_tok)))
    labels[i] <- lab_tok
    r_vals[i] <- rgb_num[1L]
    g_vals[i] <- rgb_num[2L]
    b_vals[i] <- rgb_num[3L]
  }

  all_rgb <- c(r_vals, g_vals, b_vals)
  if (any(all_rgb < 0) || any(all_rgb > 255)) {
    stop("RGB values in LUT must be within [0, 1] or [0, 255].")
  }

  is_unit_scale <- all(all_rgb <= 1) &&
    (any(all_rgb > 0 & all_rgb < 1) || identical(resolved_format, "fsl"))

  if (is_unit_scale) {
    colors <- grDevices::rgb(r_vals, g_vals, b_vals, maxColorValue = 1)
    r_int <- as.integer(round(r_vals * 255))
    g_int <- as.integer(round(g_vals * 255))
    b_int <- as.integer(round(b_vals * 255))
  } else {
    r_int <- as.integer(round(r_vals))
    g_int <- as.integer(round(g_vals))
    b_int <- as.integer(round(b_vals))
    colors <- grDevices::rgb(r_int, g_int, b_int, maxColorValue = 255)
  }

  out <- tibble::tibble(
    value = values,
    label = labels,
    r = r_int,
    g = g_int,
    b = b_int,
    color = colors
  )

  if (isTRUE(drop_zero)) {
    out <- out[out$value != 0L, , drop = FALSE]
  }

  out
}

#' Build a manual ggplot2 fill scale from a color lookup table (LUT)
#'
#' Constructs a \code{\link[ggplot2]{scale_fill_manual}} mapping region labels or integer
#' codes to the hexadecimal colors defined in a lookup table read with \code{\link{read_lut}()}.
#' Works for both \code{fill_scale} in \code{\link{geom_brain}()} and \code{outline_scale}
#' in \code{\link{geom_outline}()}.
#'
#' @param lut Either a \code{data.frame} (typically returned by \code{\link{read_lut}()})
#'   containing a \code{color} column and the column named in \code{by}, or a path to a LUT
#'   file to be parsed with \code{\link{read_lut}()}.
#' @param name Optional scale title passed to \code{\link[ggplot2]{scale_fill_manual}()}.
#'   Defaults to \code{ggplot2::waiver()}, which falls back to the layer name in \code{ggbrain}.
#' @param by Character string naming the column in \code{lut} used as keys for the color
#'   mapping. Defaults to \code{"label"} (for \code{aes(fill = label)}); use \code{"value"}
#'   when mapping integer atlas values directly (e.g., \code{aes(fill = factor(value))}),
#'   or any custom column name present in \code{lut}.
#' @param na.value Color used for missing values. Defaults to \code{"transparent"}.
#' @param ... Additional arguments passed to \code{\link[ggplot2]{scale_fill_manual}()}
#'   (e.g., \code{breaks}, \code{labels}, \code{limits}, \code{drop}, \code{guide}).
#'
#' @return A discrete \code{ggplot2} fill scale (\code{ScaleDiscrete}) mapping \code{by}
#'   keys to LUT hex colors.
#'
#' @examples
#' lut_file <- system.file("extdata", "Yeo2011_7Networks_ColorLUT.txt", package = "ggbrain")
#' yeo_lut <- read_lut(lut_file)
#' yeo_lut$network <- c("Vis", "SomMot", "DorsAttn", "SalVentAttn", "Limbic", "Cont", "Default")
#'
#' # Ready to pass to geom_brain(..., fill_scale = scale_fill_lut(yeo_lut, by = "network"))
#' sc <- scale_fill_lut(yeo_lut, name = "Yeo 7 Networks", by = "network")
#' sc
#'
#' @seealso \code{\link{read_lut}()}, \code{\link{geom_brain}()}, \code{\link{geom_outline}()}
#' @importFrom checkmate assert_data_frame assert_string assert_subset test_string
#' @importFrom ggplot2 waiver
#' @importFrom stats setNames
#' @export
scale_fill_lut <- function(lut, name = ggplot2::waiver(), by = "label", na.value = "transparent", ...) {
  if (checkmate::test_string(lut)) {
    lut <- read_lut(lut)
  }

  checkmate::assert_data_frame(lut)
  checkmate::assert_string(by)
  checkmate::assert_subset(c("color", by), names(lut))

  keys <- as.character(lut[[by]])
  cols <- as.character(lut$color)

  valid <- !is.na(keys) & !is.na(cols)
  keys <- keys[valid]
  cols <- cols[valid]

  if (length(keys) == 0L) {
    stop("LUT contains no non-missing color mappings for column '", by, "'.")
  }

  dups <- duplicated(keys)
  if (any(dups)) {
    dup_keys <- unique(keys[dups])
    conflicting <- vapply(dup_keys, function(k) {
      length(unique(toupper(cols[keys == k]))) > 1L
    }, logical(1L))
    if (any(conflicting)) {
      warning(
        "Duplicate keys in LUT column '", by, "' have conflicting colors (",
        paste(dup_keys[conflicting], collapse = ", "),
        "); keeping the first occurrence."
      )
    }
    keep <- !dups
    keys <- keys[keep]
    cols <- cols[keep]
  }

  values <- stats::setNames(cols, keys)
  ggplot2::scale_fill_manual(name = name, values = values, na.value = na.value, ...)
}
