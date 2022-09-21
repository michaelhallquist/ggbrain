#include "ggbrain.h"
//' Fast conversion of 2D mat to 3-column data.frame with dim1, dim2, value
//' 
//' @name mat2df
//' @description Converts a 2D numeric matrix into a 3-column data.frame
//' @details This function is a faster version of reshape2::melt for the simple 2-D case. It is about 2.5x faster than melt.
//' @param mat A \code{matrix} to convert to data.frame
//' @return A 3-column data.frame with dim1, dim2, and value
//' @keywords internal
//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
DataFrame mat2df(NumericMatrix mat, bool na_zeros = false) {
  int nrow = mat.nrow();
  int ncol = mat.ncol();
  //Rcout << "nrow: " << nrow << ", ncol: " << ncol << "\n";
  
  NumericMatrix df(nrow*ncol, 3);
  
  NumericMatrix::Column dim1 = df(_ , 0);  // Reference to the first column
  NumericMatrix::Column dim2 = df(_ , 1);  // Reference to the second column
  NumericMatrix::Column val  = df(_ , 2);  // Reference to the third column
  
  // passing the matrix to a numeric vector creates a proxy (reference) that does not copy mat
  NumericVector mvec(mat);
  mvec.attr("dim") = R_NilValue; // remove dimensions from mvec to ensure it is treatest as a vector
  val = mvec; // assign serialized matrix to value column of returned object
  
  if (na_zeros) {
    for (int i = 0; i < val.size(); i++) {
      if (abs(val[i]) < 1e-4) {
        val[i] = NA_REAL;
      }
    }
  }
  
  // populate dim1 and dim2 lookups by looping over row and column combinations
  int v = 0;
  for (int i = 1; i <= ncol; i++) {
    for (int j = 1; j <= nrow; j++) {
      dim1[v] = j;
      dim2[v] = i;
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

m <- matrix(rbinom(10000, size = 2, prob=0.5), nrow=100)
d <- mat2df(m)
d_z <- mat2df(m, na_zeros = TRUE)

dref <- reshape2::melt(m, varnames=c("dim1", "dim2"))

#str(m)
#str(d)
#m_recon <- df2mat(d)
library(microbenchmark)
microbenchmark(
  cpp = mat2df(m),
  cpp_z = mat2df(m, na_zeros=TRUE),
  r = reshape2::melt(m),
  k = Kmisc::melt_(m),
  dt = data.table::data.table(
    row = rep(seq_len(nrow(m)), ncol(m)), 
    col = rep(seq_len(ncol(m)), each = nrow(m)), 
    value = c(m)
  ),
  times=10000
)
*/
  