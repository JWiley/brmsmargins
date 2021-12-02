#include <RcppArmadillo.h>
#include "integratemvn.h"
#include "tab2mat.h"
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' Integrate over Random Effects
//'
//' Used to conduct Monte Carlo integration over Gaussian random effects.
//' Not intended to be called directly by most users.
//'
//' @param d A list with model matrices for each random effect block.
//' @param sd A list with standard deviation matrices for each random effect block
//'   where rows are different posterior draws.
//' @param L A list with matrices for each random effect block containing the parts of
//'   the L matrix, the Cholesky decomposition of the random effect correlation matrix.
//' @param k An integer, the number of samples for Monte Carlo integration.
//' @param yhat A matrix of the fixed effects predictions
//' @param backtrans An integer, indicating the type of back transformation.
//'   0 indicates inverse logit (e.g., for logistic regression).
//'   1 indicates exponential (e.g., for poisson or negative binomial regression or if outcome was natural log transformed).
//'   2 indicates square (e.g., if outcome was square root transformed).
//'   Any other integer may be used for no transformation.
//' @return A numeric matrix with the Monte Carlo integral calculated.
//' @export
//' @examples
//' integratere(
//'   d = list(matrix(1, 1, 1)),
//'   sd = list(matrix(1, 2, 1)),
//'   L = list(matrix(1, 2, 1)),
//'   k = 10L,
//'   yhat = matrix(0, 2, 1),
//'   backtrans = 0L)
// [[Rcpp::export]]
arma::mat integratere(List d, List sd, List L, int k, const arma::mat& yhat, int backtrans) {
  int M = yhat.n_rows;
  int N = yhat.n_cols;
  int J = sd.length();
  
  arma::mat yhat2 = arma::zeros(M, N);
  
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

    // initialize matrix for all random effect predictions
    arma::mat Zall = Z[0];
    if (J > 0) {
      for (int re = 1; re < J; re++) {
	arma::mat tmp = Z[re];
	Zall += tmp;
      }
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
