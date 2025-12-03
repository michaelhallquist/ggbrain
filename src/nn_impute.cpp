#include "ggbrain.h"

//' Imputes missing values in a 2D matrix based on the nearest non-missing neighbors in a given radius
//' @name nn_impute
//' 
//' @param in_mat a 2D matrix to fill using nearest neighbors
//' @param neighbors the number of closest non-NA neighboring values to return within \code{in_mat}. Default is 4.
//' @param radius the radius (in pixels) around each missing value to search for non-missing neighbors. Default is 8.
//' @param aggfun the function used to aggregate the neighbors in imputation. Supports "mean", "median", and "mode."
//' @param ignore_zeros if TRUE, then zero is not a valid imputation value (since these are not data in NIfTIs)
//' 
//' @details The "mode" aggfun should only be used when the matrix \code{in_mat} can be converted to integers without loss
//'   of information (i.e., the data are integerish values already).
//' @return A copy of the matrix with NA values imputed by their nearest neighbors
//' @export

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

arma::mat nn_impute(const arma::mat& in_mat, int neighbors = 4, int radius = 8, std::string aggfun = "mean", bool ignore_zeros = true) {
  // find NAs to interpolate
  mat out_mat = in_mat;
  //Rcout << "About to find non finite" << std::endl;
  uvec na_indices = find_nonfinite(in_mat);
  //Rcout << "About to call ind2sub" << std::endl;
  umat na_mat = ind2sub(size(in_mat), na_indices);
  
  // loop over missing values and replace them with neighbors
  for (int i = 0; i < na_indices.size(); i++) {
    //Rcout << "calling nearest_pts for: " << na_mat(0,i) << ", " << na_mat(1,i) << std::endl;
    arma::vec pts = nearest_pts(na_mat(0,i), na_mat(1,i), in_mat, neighbors, radius, ignore_zeros);
    
    // If no valid neighbors found, leave as NA (don't try to impute)
    if (pts.n_elem == 0) {
      continue;
    }
    
    if (aggfun == "mean") {
      out_mat(na_indices(i)) = mean(pts);
    } else if (aggfun == "median") {
      out_mat(na_indices(i)) = median(pts);
    } else if (aggfun == "mode") {
      out_mat(na_indices(i)) = integer_mode(conv_to<ivec>::from(pts)); // force conversion to integers
    } else {
      Rcpp::stop("Unable to interpret aggfun '" + aggfun + "'");
    }
  }
  
  return(out_mat);
}


// Code for testing nearest neighbor imputation
/*** R
m <- matrix(rnorm(1000), nrow=100, ncol=100)
m[sample(1:100, 10), sample(1:100, 10)] <- NA
# 
# m[20,20] <- NA

miss <- which(is.na(m), arr.ind=TRUE)
microbenchmark(
  aa_mean = ggbrain:::nn_impute(m, neighbors = 10, radius = 8, aggfun = "mean"),
  aa_median = ggbrain:::nn_impute(m, neighbors = 2, radius = 8, aggfun = "median"),
  aa_mode = ggbrain:::nn_impute(m, neighbors = 2, radius = 8, aggfun = "mode")
)
aa_mean <- ggbrain:::nn_impute(m, neighbors = 10, radius = 8, aggfun = "mean")
aa_median <- ggbrain:::nn_impute(m, neighbors = 10, radius = 8, aggfun = "median")
aa_mode <- ggbrain:::nn_impute(m, neighbors = 10, radius = 8, aggfun = "mode")

miss_aa <- which(is.na(aa), arr.ind=TRUE)
cbind(aa_mean[miss], aa_median[miss], aa_mode[miss])
*/
