#include "ggbrain.h"

//' This function flood fills a binary image with TRUE for any value of FALSE
//'
//' @name flood_fill
//' @param im A boolean matrix reference representing a binary image
//' @param x the starting x position for fill
//' @param y the starting y position for fill
//' @param r the number of rows in im
//' @param c the number of columns in im
//' @return Nothing. The matrix \code{im} is modified in place (by reference)
//'
//' @details This is an internal function used by geom_outline to clean up outlines
//' @author Michael Hallquist

// [[Rcpp::export]]

void flood_fill(LogicalMatrix& im, const int x, const int y, const int& r, const int& c) {
  if (x < 0 || x >= r || y < 0 || y >= c)
    return;
  if (im(x,y) == true)
    return; // already filled with true (no modification)
  
  //Rcout << "x is: " << x << ", y is: " << y << std::endl;
  
  // fill current position
  im(x,y) = true;
  
  // recurse to s, n, e, w
  flood_fill(im, x + 1, y, r, c); 
  flood_fill(im, x - 1, y, r, c); 
  flood_fill(im, x, y + 1, r, c); 
  flood_fill(im, x, y - 1, r, c); 
}