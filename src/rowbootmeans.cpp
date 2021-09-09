#include <Rcpp.h>
using namespace Rcpp;

//' Bootstrap row means
//'
//' This takes a numeric matrix, bootstrap resamples each row, and then
//' calculates the mean. The intended use case is for Bayesian posterior
//' predictions from sample data. Instead of directly calculating the
//' average marginal effect (AME) across all observed values, these can be
//' bootstrapped, so that uncertainty in the target population, and thus
//' the AME in the target population, can be incorporated.
//' Model uncertainty is already assumed to be handled by the different posterior
//' samples, which are assumed to be across rows.
//' 
//' @param x A numeric matrix 
//' @return A numeric vector with the simple bootstrapped row means of the matrix
//' @export
//' @examples
//'
//' x <- matrix(1:9, byrow = TRUE, 3)
//' replicate(10, rowBootMeans(x))
// [[Rcpp::export]]
NumericVector rowBootMeans(NumericMatrix x) {
  NumericVector out( x.nrow() );
  int n = x.nrow();
  for(int i = 0; i < n; i++){
    NumericVector tmp = x(i, _);
    out[i] = mean(sample(tmp, tmp.size(), true));
  }
  return out;
}
