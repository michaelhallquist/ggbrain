#include "ggbrain.h"

//' This function finds holes by flood filling TRUE into a 2D binary image, starting from the edge
//'
//' @name fill_from_edge
//' @param im A boolean matrix representing a binary image
//' @param nedges An integer specifying how many starting points along the edge to use for
//'   filling TRUE. The starts are northwest (1), southwest (2), southeast (3), and northeast (4).
//'   The compute time increases with the number of starts.
//' @return A matrix of the same size as \code{im} containing the number of neighboring pixels
//'
//' @details This is an internal function used by geom_outline to clean up outlines
//' @author Michael Hallquist

// [[Rcpp::export]]

LogicalMatrix fill_from_edge(LogicalMatrix im, int nedges=2) {
  int r = im.nrow();
  int c = im.ncol();
  
  // kick off modification of im inside worker
  flood_fill(im, 0, 0, r, c); //start in northwest
  if (nedges > 1) flood_fill(im, r-1, 0, r, c); //then southwest (zero-based indexing)
  if (nedges > 2) flood_fill(im, r-1, c-1, r, c); //then southeast
  if (nedges > 3) flood_fill(im, 0, c-1, r, c); //then northeast
  
  // not straightforward to negate matrix this way -- could just double for loop, but leaving this to R
  //https://stackoverflow.com/questions/49026407/how-can-i-do-logical-operations-on-rcppnumericmatrix-using-a-sugar-manner
  //LogicalMatrix not_im = !im; // make holes TRUE and everything else FALSE
  
  return(im);

}

/*** R
x <- matrix(rbinom(10000, size = 1, 0.5), nrow=100, ncol=100)
x[1,1] = 0
res <- fill_from_edge(x)
dim(res)
summary(as.vector(res))
*/

