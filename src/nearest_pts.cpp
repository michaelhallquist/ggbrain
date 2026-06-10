#include "ggbrain.h"

//' Finds the nearest non-missing neighbors of a target point in a 2D matrix
//' @name nearest_pts
//'
//' @param x 0-based row index of the point whose neighbors should be found within \code{in_mat}
//' @param y 0-based column index of the point whose neighbors should be found within \code{in_mat}
//' @param in_mat a 2D matrix to search for neighbors of \code{pt}
//' @param neighbors the number of closest non-NA neighboring values to return within \code{in_mat}
//' @param radius the radius around \code{pt} to search. Default: 8.
//' @param ignore_zeros if TRUE, then zero is not a valid imputation value (since these are not data in NIfTIs)
//'
//' @return A vector of \code{neighbors} closest neighboring values around \code{pt}
//' @keywords internal

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::vec nearest_pts(int x, int y, const arma::mat& in_mat, int neighbors = 4, int radius = 8, bool ignore_zeros = true) {
  // x and y are already 0-based indices (e.g., from arma::ind2sub)
  int xs = in_mat.n_rows; // size of x (rows)
  int ys = in_mat.n_cols; // size of y (cols)

  if (neighbors < 1) {
    Rcpp::stop("neighbors must be at least 1");
  }
  if (radius < 0) {
    Rcpp::stop("radius must be non-negative");
  }
  if (x < 0 || x >= xs || y < 0 || y >= ys) {
    Rcpp::stop("x and y must index a location within in_mat");
  }

  int min_x = std::max(0, x - radius);
  int max_x = std::min(xs - 1, x + radius);
  int min_y = std::max(0, y - radius);
  int max_y = std::min(ys - 1, y + radius);

  int max_candidates = (max_x - min_x + 1) * (max_y - min_y + 1) - 1;
  if (max_candidates == 0) {
    return arma::vec();
  }

  arma::vec values(max_candidates);
  arma::vec distances(max_candidates);
  arma::uword n_valid = 0;

  for (int i = min_x; i <= max_x; i++) {
    for (int j = min_y; j <= max_y; j++) {
      if (i == x && j == y) {
        continue;
      }

      double value = in_mat(i, j);
      if (!std::isfinite(value) || (ignore_zeros && std::abs(value) < 1e-4)) {
        continue;
      }

      double dx = x - i;
      double dy = y - j;
      values(n_valid) = value;
      distances(n_valid) = dx * dx + dy * dy;
      n_valid++;
    }
  }

  if (n_valid == 0) {
    return arma::vec();
  }

  arma::uvec order = arma::sort_index(distances.head(n_valid));
  arma::uword n_keep = std::min(static_cast<arma::uword>(neighbors), n_valid);
  return values.elem(order.head(n_keep));
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

m <- matrix(rnorm(1000), nrow=100, ncol=100)
m[sample(1:100, 10), sample(1:100, 10)] <- NA
#miss <- which(is.na(m), arr.ind=TRUE)
m[20,20] <- NA
#ggbrain:::nearest_pts(miss[1,], m, neighbors = 2, radius = 8)
ggbrain:::nearest_pts(20,20, m, neighbors = 8, radius = 8)
m[18:22, 18:22]

m[18:22, 18:22] <- NA
ggbrain:::nearest_pts(20,20, m, neighbors = 4, radius = 8)
m[17:23, 17:23]

microbenchmark(ggbrain:::nearest_pts(20,20, m, neighbors = 4, radius = 8))

*/
