#include "ggbrain.h"
#include <stack>

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
//' @details This is an internal function used by geom_outline to clean up outlines.
//'   Uses an iterative approach with an explicit stack to avoid stack overflow on large images.
//' @author Michael Hallquist

// [[Rcpp::export]]

void flood_fill(LogicalMatrix& im, const int x, const int y, const int& r, const int& c) {
  // Use an explicit stack to avoid call stack overflow on large images
  std::stack<std::pair<int, int>> stk;
  stk.push({x, y});
  
  while (!stk.empty()) {
    auto [cx, cy] = stk.top();
    stk.pop();
    
    // bounds check
    if (cx < 0 || cx >= r || cy < 0 || cy >= c)
      continue;
    
    // already filled
    if (im(cx, cy) == true)
      continue;
    
    // fill current position
    im(cx, cy) = true;
    
    // push neighbors (south, north, east, west)
    stk.push({cx + 1, cy});
    stk.push({cx - 1, cy});
    stk.push({cx, cy + 1});
    stk.push({cx, cy - 1});
  }
}