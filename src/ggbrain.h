#ifndef _ggbrain_GGBRAIN_h
#define _ggbrain_GGBRAIN_h

#include <RcppArmadillo.h>
#define ARMA_NO_DEBUG

using namespace Rcpp;
using namespace arma;

//function definitions

Rcpp::LogicalMatrix fill_from_edge(LogicalMatrix im, int x, int y);
void flood_fill(Rcpp::LogicalMatrix& im, const int x, const int y, const int& r, const int& c);
Rcpp::NumericMatrix count_neighbors(LogicalMatrix im, bool diagonal);
arma::mat sort_mat(arma::mat x, unsigned int col);
void print_mat(arma::mat my_matrix);
arma::vec nearest_pts(int x, int y, const arma::mat& in_mat, int neighbors = 4, int radius = 8, bool ignore_zeros = true);
int integer_mode(arma::ivec v, bool demote_zeros = true);
arma::mat nn_impute(const arma::mat& in_mat, int neighbors = 4, int radius = 8, std::string aggfun = "mean", bool ignore_zeros = true);

#endif
