#include <RcppArmadillo.h>
#include "integratemvn.h"
#include "tab2mat.h"
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

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
