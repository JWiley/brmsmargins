#include <RcppArmadillo.h>
#include "integratemvn.h"
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

//' Integrate over Random Effects
//'
//' TODO: write description.
//'
//' @param d A list
//' @param sd A list
//' @param L A list
//' @param k An integer, the number of samples
//' @param yhat A matrix of the fixed effects predictions
//' @param backtrans An integer, indicating the type of back transformation.
//'   0 indicates inverse logit (e.g., for logistic regression).
//'   1 indicates exponential (e.g., for poisson or negative binomial regression or if outcome was natural log transformed).
//'   2 indicates square (e.g., if outcome was square root transformed).
//' @return A numeric matrix with random values
//' @export
//' @examples
//'
// [[Rcpp::export]]
arma::mat integratere(List d, List sd, List L, int k, const arma::mat& yhat, int backtrans) {
  int M = yhat.n_rows;
  int N = yhat.n_cols;
  int J = sd.length();
  
  arma::mat yhat2 = arma::zeros(M, N);

  // initialize matrix for all random effect predictions
  arma::mat Zall = arma::zeros(N, k);
  
  for (int i = 0; i < M; i++) {
    List Z(J);    
    for (int re = 0; re < J; re++) {
      NumericMatrix x = L[re];
      arma::mat xmat = arma::mat(x.begin(), x.nrow(), x.ncol(), false);
      arma::mat cholmat = tab2mat(xmat.row(i));
      arma::mat dmat = d[re];

      NumericMatrix sdmat = sd[re];
      NumericVector sdvec = sdmat(i, _);

      Z[re] = integratemvn(dmat, k, sdvec, cholmat);
    }
    for (int re = 0; re < J; re++) {
      arma::mat tmp = Z[re];
      Zall += tmp;
    }
    for (int nsamp = 0; nsamp < k; nsamp++) {
      Zall.col(nsamp) = Zall.col(nsamp) + yhat.row(i).t();
    }
    if (backtrans == 0) {
      Zall = 1 / (1 + exp(-Zall));
    } else if (backtrans == 1) {
      Zall = exp(Zall);
    } else if (backtrans == 2) {
      Zall = pow(Zall, 2);
    }
    arma::colvec zm = arma::mean(Zall, 1);
    yhat2.row(i) = zm.t();
  }

  return(yhat2);
}
