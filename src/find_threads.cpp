#include "ggbrain.h"

//' This function finds 'threads' hanging off of the edges of blobs in an image, allowing the user to trim them
//'
//' @name find_threads
//' @param im A numeric matrix representing an image, with non-zero values representing pixels to display
//' @param min_neighbors the minimum number of neighbors a pixel must have to be retained
//' @param maxit the maximum number of iterations to run the thread trimming algorithm. Default: 15.
//' @param diagonal Whether to count diagonal elements as valid neighbors
//' @return A logical matrix matrix of the same size as \code{im} containing the number of neighboring pixels
//'
//' @details This algorithm runs count_neighbors iteratively until no pixel exceeds the trimming threshold \code{min_neighbors}
//'   or the maximum number of iterations, \code{maxit}, is reached.
//'
//'   By running iteratively, long tails are trimmed sequentially by pruning the most disconnected voxels.
//'
//'   The algorithm computes neighbor counts once initially, then uses incremental updates when pixels are removed.
//'   This avoids redundant full-matrix scans on each iteration, providing significant speedup for large images.
//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

LogicalMatrix find_threads(const arma::mat& img, int min_neighbors = 2, int maxit = 15, bool diagonal = false) {
  if (maxit < 1) {
    Rcpp::stop("maxit must be at least 1");
  }

  if (min_neighbors <= 0) {
    Rcpp::stop("min_neighbors must be a positive integer");
  } else if (diagonal && min_neighbors > 8) {
    Rcpp::stop("min_neighbors cannot exceed 8 for counting when diagonal is true");
  } else if (!diagonal && min_neighbors > 4) {
    Rcpp::stop("min_neighbors cannot exceed 4 for counting when diagonal is false");
  }

  int r = img.n_rows;
  int c = img.n_cols;

  arma::umat threads(r, c, fill::zeros);
  
  // Early exit: if image has 0 rows or 0 cols, return empty result
  if (r == 0 || c == 0) {
    return wrap(threads);
  }

  arma::umat img_bool(r, c, fill::zeros);
  arma::uvec nzpix = find(abs(img) > 1e-4);
  
  // Early exit: if no non-zero pixels, nothing to trim
  if (nzpix.n_elem == 0) {
    return wrap(threads);
  }
  
  img_bool.elem(nzpix).fill(1);

  // Compute neighbor counts once at the start
  arma::imat neighbors = count_neighbors(img_bool, diagonal);

  // Build offset list for neighbor positions
  std::vector<std::pair<int,int>> offsets;
  offsets.push_back({-1, 0});  // north
  offsets.push_back({1, 0});   // south
  offsets.push_back({0, -1});  // west
  offsets.push_back({0, 1});   // east
  if (diagonal) {
    offsets.push_back({-1, -1}); // northwest
    offsets.push_back({-1, 1});  // northeast
    offsets.push_back({1, -1});  // southwest
    offsets.push_back({1, 1});   // southeast
  }

  bool pixels_remain = true;
  int it = 0;

  while (pixels_remain && it < maxit) {
    // Find pixels below threshold (but not already removed/empty)
    arma::uvec below_thresh = find(neighbors < min_neighbors && neighbors > INT_MIN);

    if (below_thresh.size() > 0) {
      // Process each removed pixel and update neighbor counts incrementally
      for (arma::uword k = 0; k < below_thresh.size(); k++) {
        // Convert linear index to row/col subscripts (Armadillo uses column-major order)
        int pi = below_thresh(k) % r;  // row
        int pj = below_thresh(k) / r;  // col

        // Mark pixel as removed
        img_bool(pi, pj) = 0;
        threads(pi, pj) = 1;
        neighbors(pi, pj) = INT_MIN;

        // Decrement neighbor count for all adjacent active pixels
        for (const auto& off : offsets) {
          int ni = pi + off.first;
          int nj = pj + off.second;
          if (ni >= 0 && ni < r && nj >= 0 && nj < c && img_bool(ni, nj) == 1) {
            neighbors(ni, nj)--;
          }
        }
      }
    } else {
      pixels_remain = false;
    }

    it++;
  }

  // use wrap to return logical matrix from umat
  return(wrap(threads));
}

/*** R
x <- matrix(rbinom(10000, size = 1, 0.5), nrow=100, ncol=100)
x <- matrix(rnorm(10000), nrow=100, ncol=100)
x[abs(x) < 1] <- 0
library(imager)
plot(as.cimg(x))
#system.time(res <- count_neighbors(x, FALSE))
#dim(res)
test_square <- ggbrain:::find_threads(x, min_neighbors = 2, maxit = 100, diagonal = FALSE)
test_diag <- ggbrain:::find_threads(x, min_neighbors = 3, maxit = 100, diagonal = TRUE)
plot(as.cimg(test_square))
plot(as.cimg(test_diag))
plot(as.cimg(x) - as.cimg(test_diag))
plot(as.cimg(x*(1-test_diag))) #just areas that survive connection tests

neigh_orig <- ggbrain:::count_neighbors(x, diagonal = TRUE)
neigh_square <- ggbrain:::count_neighbors(test_square, diagonal = TRUE)
neigh_diag <- ggbrain:::count_neighbors(test_diag, diagonal = TRUE)
table(neigh_orig, useNA="always")
table(neigh_square, useNA="always")
table(neigh_diag, useNA="always")
library(microbenchmark)
microbenchmark(
  test_square = ggbrain:::find_threads(x, min_neighbors = 2, maxit = 100, diagonal = FALSE),
  test_diag = ggbrain:::find_threads(x, min_neighbors = 3, maxit = 100, diagonal = TRUE)
)
*/
