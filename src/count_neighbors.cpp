#include "ggbrain.h"

//' This function counts the number of neighboring/touching pixels in a 2D binary image
//'
//' @name count_neighbors
//' @param im A boolean matrix representing a binary image
//' @param diagonal Whether to count diagonal elements as valid neighbors
//' @return A matrix of the same size as \code{im} containing the number of neighboring pixels
//'
//' @details This is an internal function used by geom_outline to clean up outlines
//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::imat count_neighbors(const arma::umat& im, bool diagonal = true) {
  int r = im.n_rows;
  int c = im.n_cols;
  
  // Rcout << "rows: " << r << ", cols: " << c;
  
  int n = 0;
  arma::imat neighbors(r, c, fill::zeros);
  
  for (int i = 0; i < r; i++) {
    for (int j = 0; j < c; j++) {
      n = 0; // reset count
      
      // if pixel is FALSE (empty), neighbor count is always NA
      if (im(i, j) == 0) {
        //Rcout << "setting nan i: " << i << ", j: " << j << endl;
        // integer matrices don't support nans
        neighbors(i, j) = INT_MIN;
        continue;
      }
      
      // north
      if (i > 0 && im(i - 1, j) == 1)
        n++;
      
      // northeast
      if (diagonal && i > 0 && j < c - 1 && im(i - 1, j + 1) == 1)
        n++;
      
      // east
      if (j < r - 1 && im(i, j + 1) == 1)
        n++;
      
      // southeast
      if (diagonal && i < r - 1 && j < c - 1 && im(i + 1, j + 1) == 1)
        n++;
      
      // south
      if (i < r - 1 && im(i + 1, j) == 1)
        n++;
      
      // southwest
      if (diagonal && i < r - 1 && j > 0 && im(i + 1, j - 1) == 1)
        n++;
      
      // west
      if (j > 0 && im(i, j - 1) == 1)
        n++;
      
      // northwest
      if (diagonal && i > 0 && j > 0 && im(i - 1, j - 1) == 1)
        n++;
      
      neighbors(i, j) = n;
    }
  }
  
  // hasnan doesn't work for integer matrices
  // Rcout << "count_neighbors hasnan: " << neighbors.has_nan() << endl;
  //neighbors(1,1) = datum::inf;
  //Rcout << "neigh(1,1): " << neighbors(1,1) << endl;
  //Rcout << "neigh(99,99): " << neighbors(99,99) << endl;
  
  // arma::uvec miss = find_nonfinite(neighbors);
  // arma::uvec present = find_finite(neighbors);
  // arma::uvec int_min = find(neighbors == INT_MIN);
  // Rcout << "non_finite: " << miss.n_elem << endl;
  // Rcout << "finite: " << present.n_elem << endl;
  // Rcout << "int_min: " << int_min.n_elem << endl;
  
  return(neighbors);
  
}

/*** R
set.seed(1001)
x <- matrix(rbinom(10000, size = 1, 0.5), nrow=100, ncol=100)
system.time(res <- count_neighbors(x))
system.time(res <- count_neighbors(x, FALSE))
dim(res)
summary(as.vector(res))
*/
