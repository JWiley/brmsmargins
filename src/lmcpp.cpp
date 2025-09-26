#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Fast Linear Regression
//'
//' Used to get marginal coefficients off of a generalized linear mixed model.
//'
//' @param X A numeric model matrix. If intercept is desired, it must already have been added as a column.
//' @param y A numeric matrix. A single column if one response variable or multiple columns
//'   where each column is a different response, such as a for marginal coefficients where
//'   each column is a different MCMC sample.
//' @return A numeric matrix with the coefficient.
//' @export
//' @examples
//' lmcpp(cbind(1, mtcars$hp, mtcars$am), as.matrix(mtcars[, c("mpg", "qsec")]))
// [[Rcpp::export]]
arma::mat lmcpp(const arma::mat& X, const arma::mat& y) {
  arma::mat Q, R;
  arma::qr_econ(Q, R, X);                 // X = Q * R, R upper-triangular
  arma::mat QtY = Q.t() * y;              // Project all response columns at once
  arma::mat B = arma::solve(trimatu(R), QtY, arma::solve_opts::fast); // Backsolve
  return B;
}
