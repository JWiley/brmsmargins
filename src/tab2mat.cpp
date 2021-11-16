#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Convert a row of a table to a matrix
//'
//' TODO: write description.
//'
//' @param X a matrix
//' @return A numeric matrix with one row.
//' @export
//' @examples
//'
// [[Rcpp::export]]
arma::mat tab2mat(const arma::mat& X) {
  int ncol = X.n_cols;
  double dims = sqrt(ncol); 
  arma::mat Z = arma::zeros(dims, dims);
  
  for (int i = 0; i < dims; i++) {
    for (int j = 0; j < dims; j++) {
      Z(i, j) = X(0, j + dims * i);
    }
  }
  return(Z);
}
