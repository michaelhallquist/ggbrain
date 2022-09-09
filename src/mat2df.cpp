#include "ggbrain.h"
//' Fast conversion of 2D mat to 3-column data.frame with dim1, dim2, value
//' 
//' @name mat2df
//' @description Converts a 2D numeric matrix into a 3-column data.frame
//'
//' @param mat   A \code{matrix} to convert to data.frame
//' @return A 3-column data.frame with dim1, dim2, and value
//' @keywords internal
//' @author Michael Hallquist

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
DataFrame mat2df(const arma::mat& mat) {
  int nrow = mat.n_rows;
  int ncol = mat.n_cols;
  //Rcout << "nrow: " << nrow << ", ncol: " << ncol << "\n";
  
  // IntegerVector dim1 = (nrow*ncol);
  // IntegerVector dim2 = (nrow*ncol);
  
  arma::mat df(nrow*ncol, 3);
  
  arma::vec dim1 (df.colptr(0), nrow*ncol, false);
  arma::vec dim2 (df.colptr(1), nrow*ncol, false);
  arma::vec val (df.colptr(2), nrow*ncol, false);
  val = mat.as_col();
  
  // IntegerVector d1 = rep(seq_len(nrow), ncol);
  // IntegerVector d2 = rep_each(seq_len(ncol), nrow);
  // 
  // dim1 = Rcpp::as<vec>(d1);
  // dim2 = Rcpp::as<vec>(d2);
  
  int v = 0;
  for (int i = 1; i <= nrow; i++) {
    for (int j = 1; j <= ncol; j++) {
      dim1(v) = j;
      dim2(v) = i;
      v++;
    }
  }
  
   
  // DataFrame df = DataFrame::create(Named("dim1") = dim1, Named("dim2") = dim2, Named("value") = mat.as_col());
  //NumericMatrix df = cbind(dim1, dim2, x);
  // arma::mat df(nrow*ncol, 3);
  // df.col(0) = dim1;
  // df.col(1) = dim2;
  // df.col(2) = mat.as_col();
  
  // list backdoor approach (not very fast)
  // Rcpp::List df(3);
  // df.names() = Rcpp::CharacterVector::create("dim1", "dim2", "value");
  // 
  // df["dim1"] = dim1;
  // df["dim2"] = dim2;
  // df["value"] = mat.as_col();
  // 
  // df.attr("class") = Rcpp::CharacterVector::create("data.frame");
  // 
  
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
d <- armamat2df(m)
library(microbenchmark)
microbenchmark(
  armacpp = armamat2df(m),
  cpp = mat2df(m),
  r = reshape2::melt(m),
  times=10000
)

*/
  