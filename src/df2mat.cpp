#include "ggbrain.h"
//' Convert a 3-column data.frame (dim1, dim2, value) to a 2-D matrix
//' 
//' @name df2mat
//'
//' @param df   A \code{data.frame} representing a melted 2-D matrix, having columns dim1, dim2, and value
//' @param replace_na if not \code{NULL}, this numeric value will be used to replace any NAs in \code{df} in the
//'   resulting matrix. This is useful if downstream code is not built to handle missing values.
//' @details
//' There is virtually no input validation of \code{df}. You must pass a data.frame that has dim1, dim2, and value as
//'   columns. Otherwise, it will not work as expected.
//'
//' This is a much faster version of the acast function from \code{reshape2} that works only on 2-D matrix conversions.
//' @return The matrix form of the keyed data.frame object
//' @keywords internal

// [[Rcpp::export]]
NumericMatrix df2mat(const DataFrame& df, Nullable<NumericVector> replace_na = R_NilValue) {
  
  // Rcpp uses references, not copies, of the columns: https://teuder.github.io/rcpp4everyone_en/140_dataframe.html
  IntegerVector dim1 = df["dim1"];
  IntegerVector dim2 = df["dim2"];
  NumericVector value = df["value"];
  
  int nrow = max(dim1);
  int ncol = max(dim2);
  bool repna = false;
  double repval;
  if (replace_na.isNotNull()) {
    NumericVector r(replace_na); // cast nullable to numeric vector
    repval = r(0); // get numeric value
    repna = true;
  }
  
  double v;
  
  // populate matrix using dim1 and dim2 keys
  NumericMatrix mat(nrow, ncol);
  for (int i = 0; i < df.nrow(); i++) {
    if (repna && NumericVector::is_na(value[i])) {
      v = repval;
    } else {
      v = value(i);
    }
    mat(dim1(i) - 1, dim2(i) - 1) = v;
  }

  return mat;
}

/*** R
m <- matrix(1:100, nrow=10)
m[3:5, 4:6] <- NA # test replace_na
#d <- mat2df(m)
d <- reshape2::melt(m, varnames=c("dim1", "dim2"))
m_recon <- df2mat(d)
all.equal(m, m_recon)

m_recon_rep <- df2mat(d, replace_na = 0)
library(microbenchmark)
 microbenchmark(
   cpp = df2mat(d),
   cpp_rep = df2mat(d, replace_na=0),
   r = reshape2::acast(d, dim1 ~ dim2),
   times=1000
)
*/
