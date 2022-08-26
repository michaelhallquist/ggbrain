#ifndef _ggbrain_GGBRAIN_h
#define _ggbrain_GGBRAIN_h

#include <Rcpp.h>
using namespace Rcpp;

//function definitions

Rcpp::LogicalMatrix fill_from_edge(LogicalMatrix im, int x, int y);
void flood_fill(Rcpp::LogicalMatrix& im, const int x, const int y, const int& r, const int& c);
Rcpp::NumericMatrix count_neighbors(LogicalMatrix im, bool diagonal);

#endif
