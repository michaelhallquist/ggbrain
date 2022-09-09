#include "ggbrain.h"
//' Fast conversion of 2D mat to 3-column data.frame with dim1, dim2, value
//' 
//' @name mat2df
//' @description Converts a 2D numeric matrix into a 3-column data.frame
//' @details This function is a faster version of reshape2::melt for the simple 2-D case. It is about 2x faster than melt.
//' @param mat A \code{matrix} to convert to data.frame
//' @return A 3-column data.frame with dim1, dim2, and value
//' @keywords internal
//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
DataFrame mat2df(const arma::mat& mat) {
  int nrow = mat.n_rows;
  int ncol = mat.n_cols;
  //Rcout << "nrow: " << nrow << ", ncol: " << ncol << "\n";
  
  arma::mat df(nrow*ncol, 3);
  
  arma::vec dim1 (df.colptr(0), nrow*ncol, false);
  arma::vec dim2 (df.colptr(1), nrow*ncol, false);
  arma::vec val (df.colptr(2), nrow*ncol, false);
  val = mat.as_col();
  
  int v = 0;
  for (int i = 1; i <= nrow; i++) {
    for (int j = 1; j <= ncol; j++) {
      dim1(v) = j;
      dim2(v) = i;
      v++;
    }
  }
  
  // convert matrix to data.frame, then set names. This is faster than the ::create constructor
  DataFrame ret = DataFrame(df);
  ret.attr("names") = Rcpp::CharacterVector::create("dim1", "dim2", "value");
  
  return(ret);
}

/*** R
m <- matrix(1:10000, nrow=100)
#d <- mat2df(m)
#str(m)
#str(d)
#m_recon <- df2mat(d)
library(microbenchmark)
microbenchmark(
  cpp = mat2df(m),
  r = reshape2::melt(m),
  dt = data.table(
    row = rep(seq_len(nrow(m)), ncol(m)), 
    col = rep(seq_len(ncol(m)), each = nrow(m)), 
    value = c(m)
  ),
  times=10000
)
*/
  