#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' To write
//'
//' TODO: write description.
//'
//' @param X a matrix
//' @return A numeric matrix
//' @export
//' @examples
//'
// [[Rcpp::export]]
arma::mat tab2mat(arma::mat X) {
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

//' To write
//'
//' TODO: write description.
//'
//' @param obj A list
//' @return A numeric matrix with random values
//' @export
//' @examples
//'
// [[Rcpp::export]]
arma::mat integratere(List obj) {
  arma::mat Z = arma::randn(1, 1);
  for (int i = 0; i < 3; i++) {
    Z(0, 0) += obj[i];
  }
  return(Z);
}


// dims <- 3
// for (i in 0:2) {
//   for (j in 0:2) {
//     print(j + dims * i)
//       }
//  }
