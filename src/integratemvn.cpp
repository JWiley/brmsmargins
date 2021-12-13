#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Integrate over Multivariate Normal Random Effects
//'
//' Used in the process of Monte Carlo integration
//' over multivariate normal random effects. This generates the
//' random draws from the multivariate normal distribution
//' and multiplies these by the data.
//' Not intended to be called directly by most users.
//'
//' @param X A numeric matrix of the data to be multiplied by the random effects
//' @param k An integer, the number of random samples to be used for numerical integration
//' @param sd A numeric vector of the standard deviations
//' @param chol A numeric matrix, which should be the Cholesky decomposition of the
//'   correlation matrix of the multivariate normal distribution.
//' @return A numeric matrix with random values
//' @export
//' @examples
//' integratemvn(
//'   X = matrix(1, 1, 2),
//'   k = 100L,
//'   sd = c(10, 5),
//'   chol = chol(matrix(c(1, .5, .5, 1), 2)))
//' 
//' integratemvn(matrix(1, 1, 1), 100L, c(5), matrix(1))
// [[Rcpp::export]]
arma::mat integratemvn(const arma::mat& X, const int k, const Rcpp::NumericVector& sd, const arma::mat& chol) {
  int n = sd.length();
  arma::mat Z = arma::randn(k, n);
  if (n > 1) {
    Z = Z * chol;
  }
  for (int i = 0; i < n; i++) {
    Z.col(i) *= sd(i);
  }
  arma::mat out = X * Z.t();  
  return(out);
}
