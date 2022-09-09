#include "ggbrain.h"
//' Convert a 3-column data.frame (dim1, dim2, value) to a 2-D matrix
//' 
//' @name df2mat
//'
//' @param x   A \code{matrix} to sort
//' @param col A \code{int} that indicates the column the matrix should sort by.
//' @details
//' There is virtually no input validation of \code{df}. You must pass a data.frame that has dim1, dim2, and value as
//'   columns. Otherwise, it will not work as expected.
//'
//' This is a much faster version of the acast function from \code{reshape2} that works only on 2-D matrix conversions.
//' @return The matrix form of the keyed data.frame object
//' @keywords internal

// [[Rcpp::export]]
NumericMatrix df2mat(const DataFrame& df) {
  
  // Rcpp uses references, not copies, of the columns: https://teuder.github.io/rcpp4everyone_en/140_dataframe.html
  IntegerVector dim1 = df["dim1"];
  IntegerVector dim2 = df["dim2"];
  NumericVector value = df["value"];
  
  int nrow = max(dim1);
  int ncol = max(dim2);
  
  // populate matrix using dim1 and dim2 keys
  NumericMatrix mat(nrow, ncol);
  for (int i = 0; i < df.nrow(); i++) {
    mat(dim1(i) - 1, dim2(i) - 1) = value(i);
  }

  return mat;
}

/*** R
m <- matrix(1:100, nrow=10)
d <- mat2df(m)
m_recon <- df2mat(d)
identical(m, m_recon)
library(microbenchmark)
 microbenchmark(
   cpp = df2mat(d),
   r = reshape2::acast(d, dim1 ~ dim2),
   times=1000
)
*/
