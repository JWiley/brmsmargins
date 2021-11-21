#'  Internal function to average over posterior predictions
#'
#' TODO
#'
#' @param posterior A posterior matrix type object. It is assumed that different
#'   predictions to be averaged over are on different columns. Different posterior
#'   draws are on different rows.
#' @param resample An integer indicating the number of
#'   bootstrap resamples of the posterior predictions to
#'   use when calculating summaries. Defaults to \code{0L}.
#'   See the details section for more informations as its implementation
#'   is experimental and it may not operate as one would expect.
#' @param seed A seed for random number generation. Defaults to \code{FALSE},
#'   which means no seed is set.
#'   Only used if \code{resample} is a positive, non-zero integer.
#' @return A vector of the averaged posterior.
#' @keywords internal
.averagePosterior <- function(posterior, resample = 0L, seed = FALSE) {
  if (isTRUE(resample == 0)) {
    posterior <- rowMeans(posterior, na.rm = TRUE)
  } else if (isTRUE(resample > 0)) {
    if (!isFALSE(seed)) {
      set.seed(seed)
    }

    yhat <- matrix(NA_real_, nrow = nrow(posterior), ncol = resample)
    for (i in seq_len(resample)) {
      yhat[, i] <- rowBootMeans(posterior)
    }

    posterior <- as.vector(yhat)
  }
  return(posterior)
}
