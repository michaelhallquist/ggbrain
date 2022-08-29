#include "ggbrain.h"

//' Finds the nearest non-missing neighbors of a target point in a 2D matrix
//' @name nearest_pts
//' 
//' @param x x-position of the point whose neighbors should be found within \code{in_mat}
//' @param y y-position of the point whose neighbors should be found within \code{in_mat}
//' @param in_mat a 2D matrix to search for neighbors of \code{pt}
//' @param neighbors the number of closest non-NA neighboring values to return within \code{in_mat}
//' @param radius the radius around \code{pt} to search.
//' 
//' @return A vector of \code{neighbors} closest neighboring values around \code{pt}
//' @keywords internal

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::vec nearest_pts(int x, int y, const arma::mat& in_mat, int neighbors, int radius) {
  x = x - 1; // x position (subtract 1 to obtain 0-based index)
  y = y - 1; // y position
  int xs = in_mat.n_rows; // size of x (rows)
  int ys = in_mat.n_cols; // sizy of y (cols)
  
  int min_x = std::max(0, x - radius);
  int max_x = std::min(xs - 1, x + radius);
  int min_y = std::max(0, y - radius);
  int max_y = std::min(ys - 1, y + radius);
  //Rcout << "min_x: " << min_x << ", max_x: " << max_x << ", min_y: " << min_y << ", max_y: " << max_y << std::endl;
  
  arma::mat search = in_mat.submat(min_x, min_y, max_x, max_y);
  arma::mat dists(search.n_rows*search.n_cols, 3);
  arma::rowvec rd(3);
  int r = 0;
  
  for (int i = 0; i < search.n_rows; i++) {
    //Rcout << "i: " << i << std::endl;
    rd(0) = i;
    for (int j = 0; j < search.n_cols; j++) {
      //Rcout << "j: " << j << std::endl;
      rd(1) = j;
      
      if ((min_x + i) == x && (min_y + j) == y) {
        rd(2) = datum::inf; // set infinite distance to self coordinate to remove it from consideration
      } else if (!std::isnan(search(i,j))) {
        rd(2) = sqrt(pow(x-(min_x+i), 2) + pow(y-(min_y+j), 2));
      } else {
        rd(2) = datum::inf; // set to infinite distance so that it sorts to the bottom
      }
      
      dists.row(r) = rd;
      r++;
    }
  }
  
  //Rcout << "About to sort matrix" << std::endl;
  
  // sort distance matrix from nearest to furthest
  dists = sort_mat(dists, 2);
  //print_mat(dists);
  
  arma::mat keep = dists.rows(0, neighbors - 1);
  //print_mat(keep);
  
  // lookup positions of closesnt neighbors within the search matrix
  // sub2ind requires 2 rows, with first being the row for .elem and the second being the column
  arma::umat locs = arma::conv_to<arma::umat>::from(keep.cols(0, 1)).t();
  arma::uvec ret = sub2ind(size(search), locs);
  
  // return the values at the nearest non-NA locations
  return(search.elem(ret));
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
