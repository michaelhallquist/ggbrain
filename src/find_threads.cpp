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
    Rcpp::stop("min_neighbors cannot exceed 8 for diagonal counting");
  } else if (!diagonal && min_neighbors > 4) {
    Rcpp::stop("min_neighbors cannot exceed 8 for diagonal counting");
  }

  arma::umat threads(img.n_rows, img.n_cols, fill::zeros);
  arma::umat img_bool(img.n_rows, img.n_cols, fill::zeros);
  arma::uvec nzpix = find(abs(img) > 1e-4);
  img_bool.elem(nzpix).fill(1);

  bool pixels_remain = true;
  int it = 0;

  while (pixels_remain && it < maxit) {
    arma::imat neighbors = count_neighbors(img_bool, diagonal);

    //Rcout << "it: " << it << endl;
    //Rcout << "hasnan: " << neighbors.has_nan() << endl;

    // Armadillo doesn't support NaN for integer matrices, so we use INT_MIN
    arma::uvec below_thresh = find(neighbors < min_neighbors && neighbors > INT_MIN);

    //arma::uvec miss = find(neighbors == INT_MIN);
    //Rcout << "nonfinite size: " << miss.n_elem << endl;
    if (below_thresh.size() > 0) {
      //Rcout << "size: " << below_thresh.size() << endl;
      img_bool.elem(below_thresh).fill(0); // set below thresh pixels to false
      threads.elem(below_thresh).fill(1); // in threads matrix, add the below-threshold pixels to set to be removed
    } else {
      pixels_remain = false;
    }

    //pixels_remain = any(vectorise(neighbors) > 0);
    it++;
  }

  // these attempts at conversion to LogicalMatrix from umat fail
  //LogicalMatrix tlog = Rcpp::as<Rcpp::LogicalMatrix>(threads);
  //LogicalMatrix tlog(threads.begin(), threads.end());

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
