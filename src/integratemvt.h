#ifndef __integratemvt__
#define __integratemvt__

#include <RcppArmadillo.h>
arma::mat integratemvt(const arma::mat& X, const arma::uword& k, const arma::rowvec& sd,
                       const arma::mat& chol, const double& df);

#endif // __integratemvt__
