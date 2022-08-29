#include "ggbrain.h"

void print_mat(arma::mat my_matrix) {
  
  uint cols = my_matrix.n_cols;
  uint rows = my_matrix.n_rows;
  
  Rcout << "--------\n";
  for(uint rX = 0; rX < rows; rX++) {
    Rcout << " " << rX << ": ";
    for(uint cX = 0; cX < cols; cX++) {
      Rcout << my_matrix(rX, cX) << " ";
    }
    Rcout << "\n";
  }
  Rcout << "--------\n";
}
