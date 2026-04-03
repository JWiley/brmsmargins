#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Integrate over Multivariate Student-t Random Effects
//'
//' Used in the process of Monte Carlo integration
//' over multivariate Student-t random effects. This generates the
//' random draws from the multivariate Student-t distribution
//' and multiplies these by the data.
//' Not intended to be called directly by most users.
//'
//' @param X A numeric matrix of the data to be multiplied by the random effects
//' @param k An integer, the number of random samples to be used for numerical integration
//' @param sd A numeric vector of the standard deviations
//' @param chol A numeric matrix, which should be the Cholesky decomposition of the
//'   correlation matrix of the multivariate Student-t distribution.
//' @param df A numeric scalar giving the degrees of freedom of the
//'   (multivariate) Student-t distribution.
//' @return A numeric matrix with random values
//' @export
//' @examples
//' integratemvt(
//'   X = matrix(1, 1, 2),
//'   k = 100L,
//'   sd = c(10, 5),
//'   chol = chol(matrix(c(1, .5, .5, 1), 2)),
//'   df = 5)
//'
//' integratemvt(matrix(1, 1, 1), 100L, c(5), matrix(1), df = 5)
// [[Rcpp::export]]
arma::mat integratemvt(const arma::mat& X, const arma::uword& k, const arma::rowvec& sd,
                       const arma::mat& chol, const double& df) {
  arma::mat Z(k, sd.size(), arma::fill::randn);
  NumericVector w_nv = Rcpp::rchisq(k, df);
  arma::vec w(w_nv.begin(), w_nv.size(), false, true);

  Z *= chol;
  Z.each_col() %= arma::sqrt(df / w);
  Z.each_row() %= sd;

  return X * Z.t();
}
