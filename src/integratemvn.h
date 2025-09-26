#ifndef __integratemvn__
#define __integratemvn__

#include <RcppArmadillo.h>
arma::mat integratemvn(const arma::mat& X, const arma::uword&, const arma::rowvec& sd, const arma::mat& chol);

#endif // __integratemvn__
