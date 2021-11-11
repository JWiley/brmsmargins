#ifndef __integratemvn__
#define __integratemvn__

#include <RcppArmadillo.h>
arma::mat integratemvn(arma::mat X, int k, Rcpp::NumericVector sd, arma::mat chol);

#endif // __integratemvn__
