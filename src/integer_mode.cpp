#include "ggbrain.h"

//' Finds the mode of an integer vector
//' @name integer_mode
//' @param v a vector of integers
//' @param demote_zeros if TRUE, zero will not be allowed to be the mode
//' 
//' @return An integer representing the mode of the vector. If multiple modes are found, then the lowest-valued mode
//'   is returned
//' @keywords internal

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]

int integer_mode(arma::ivec v, bool demote_zeros) {
  v = arma::sort(v); // sort vector to allow for grouped counting
  int number; // the current value being evaluated as a candidate mode
  int start; //starting position of the current candidate for mode within the vector
  
  int mode = v[0]; //initialize mode of chunk to first element of sorted vector
  int largest_count = 0; //number to beat (current winning mode)
  int this_count = 0;
  
  int j = 0;
  while (j < v.size()) {
    number = v[j];
    start  = j;
    
    //grouped loop over all values of this number
    while(v[j] == number && j <= v.size()) { j++; }
    
    this_count = j - start;
    
    if (this_count > largest_count || (demote_zeros && mode == 0)) {
      mode = number;
      largest_count = this_count;
    }
  }
  
  return(mode);
}


/*** R
integer_mode(c(1:10000, 5, 50, 10, 100, 50))
microbenchmark(integer_mode(c(1:10000, 10, 100)))
*/


