#include "ggbrain.h"

//' This function counts the number of neighboring/touching pixels in a 2D binary image
//'
//' @name count_neighbors
//' @param im A boolean matrix representing a binary image
//' @param diagonal Whether to count diagonal elements as valid neighbors
//' @return A matrix of the same size as \code{im} containing the number of neighboring pixels
//'
//' @details This is an internal function used by geom_outline to clean up outlines
//' @author Michael Hallquist

// [[Rcpp::export]]

NumericMatrix count_neighbors(LogicalMatrix im, bool diagonal = true) {
  int r = im.nrow();
  int c = im.ncol();
  
  // Rcout << "rows: " << r << ", cols: " << c;
  
  int n = 0;
  NumericMatrix neighbors(r, c);
  
  for (int i = 0; i < r; i++) {
    for (int j = 0; j < c; j++) {
      n = 0; // reset count
      
      // north
      if (i > 0 && im(i - 1, j) == 1)
        n++;
      
      // northeast
      if (diagonal && i > 0 && j < c - 1 && im(i - 1, j + 1) == 1)
        n++;
      
      // east
      if (j < r - 1 && im(i, j + 1) == 1)
        n++;
      
      // southeast
      if (diagonal && i < r - 1 && j < c - 1 && im(i + 1, j + 1) == 1)
        n++;
      
      // south
      if (i < r - 1 && im(i + 1, j) == 1)
        n++;
      
      // southwest
      if (diagonal && i < r - 1 && j > 0 && im(i + 1, j - 1) == 1)
        n++;
      
      // west
      if (j > 0 && im(i, j - 1) == 1)
        n++;
      
      // northwest
      if (diagonal && i > 0 && j > 0 && im(i - 1, j - 1) == 1)
        n++;
      
      neighbors(i, j) = n;
    }
  }
  
  return(neighbors);
  
}

/*** R
x <- matrix(rbinom(10000, size = 1, 0.5), nrow=100, ncol=100)
system.time(res <- count_neighbors(x))
system.time(res <- count_neighbors(x, FALSE))
dim(res)
summary(as.vector(res))
*/
