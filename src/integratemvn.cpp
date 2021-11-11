#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Integrate over multivariate normal random effects
//'
//' TODO: write description.
//'
//' @param X A numeric matrix of the data to be multiplied by the random effects
//' @param k An integer, the number of random samples to be used for numerical integration
//' @param sd A numeric vector of the standard deviations
//' @param chol A numeric matrix, which should be the Cholesky decomposition of the
//'   correlation matrix of the multivariate normal distribution.
//' @return A numeric matrix with random values
//' @export
//' @examples
//'
//' integratemvn(matrix(1, 1, 2), 100L, c(10, 5), chol(matrix(c(1, .5, .5, 1), 2)))
//' integratemvn(matrix(1, 1, 1), 100L, c(5), matrix(1))
// [[Rcpp::export]]
arma::mat integratemvn(arma::mat X, int k, Rcpp::NumericVector sd, arma::mat chol) {
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
