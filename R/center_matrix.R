#' helper function to center a numeric matrix within a (larger) target matrix size
#' @param output_dim the desired dimensions of the output matrix
#' @param mat the 2D numeric matrix containing values to be centered in the output matrix
#' @param default_value the value that should fill padded rows and columns of the output matrix
#' @param drop_zeros if TRUE, all zero-valued rows and columns of \code{mat} will be dropped before
#'   the data are centered within the output matrix. This is useful if the matrix is asymmetric,
#'   but you still want to have it be dead-center in the output.
#' @return an expanded matrix of size \code{output_dim} with the input matrix \code{mat} centered
#'   within it.
#' @keywords internal
center_matrix <- function(output_dim, mat, default_value = NA_real_, drop_zeros = TRUE) {
  checkmate::assert_matrix(mat)

  # if there are blank slices, drop them out before centering (in case original is asymmetric)
  if (isTRUE(drop_zeros)) {
    nz_mat <- 1L * (mat > 1e-5 | mat < -1e-5)
    nz_rows <- rowSums(nz_mat, na.rm = TRUE) > 0
    nz_cols <- colSums(nz_mat, na.rm = TRUE) > 0
    #lzr <- min(which(nz_rows == TRUE)) - 1
    #rzr <- length(nz_rows) - max(which(nz_rows == TRUE))
    #lzr_bias <- lzr - rzr
    mat <- mat[nz_rows, nz_cols, drop=FALSE]
  }

  d_sm <- dim(mat)
  if (any(d_sm > output_dim)) {
    stop("mat matrix is larger in at least one dimension than output_dim")
  }

  d1_diff <- output_dim[1L] - d_sm[1L]
  d2_diff <- output_dim[2L] - d_sm[2L]

  if (d1_diff > 1L) {
    d1_offset <- round(d1_diff/2) # center in x
  } else {
    d1_offset <- 0
  }

  if (d2_diff > 1L) {
    d2_offset <- round(d2_diff / 2) # center in y
  } else {
    d2_offset <- 0
  }

  d_cent <- array(default_value, dim = output_dim)
  d_cent[(seq_len(d_sm[1L]) + d1_offset), (seq_len(d_sm[2L]) + d2_offset)] <- mat
  return(d_cent)
}