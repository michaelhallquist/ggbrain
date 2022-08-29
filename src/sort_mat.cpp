#include "ggbrain.h"
// Grabbed from simts package: https://github.com/SMAC-Group/simts

//' Sort Matrix by Column
//' 
//' @name sort_mat
//' @description Sorts a given matrix by a specific column while retain the elements in each row.
//'
//' @param x   A \code{matrix} to sort
//' @param col A \code{int} that indicates the column the matrix should sort by.
//' @details
//' This functions sorts a matrix based on one column, keeping the rows together.
//' Note that \code{col} should be a zero-based index of \code{x} (i.e., first column is 0).
//' @return The matrix sorted by values in the specified column.
//' @keywords internal

// [[Rcpp::export]]
arma::mat sort_mat(arma::mat x, unsigned int col) {

  arma::uvec id = arma::sort_index(x.col(col));

  for (unsigned int i = 0; i < x.n_cols; i++){
    arma::vec sub = x.col(i);
    x.col(i) = sub.elem(id);
  }

  return x;
}
