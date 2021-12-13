#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Convert a Row of a Table to a Square Matrix
//'
//' Utility function to convert a row matrix to a square matrix.
//' Used as the \code{brms} package returns things like the Cholesky
//' decomposition matrix as separate columns where rows are posterior draws.
//' Not intended to be called directly by most users.
//'
//' @param X a matrix
//' @return A numeric matrix with one row.
//' @export
//' @examples
//' tab2mat(matrix(1:4, 1))
//' tab2mat(matrix(1:9, 1))
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
