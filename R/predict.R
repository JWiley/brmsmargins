#' Function to generate posterior predictions and summaries from a brms model
#'
#' This is an internal function that is essentially a fancy wrapper for
#' \code{fitted()}.
#'
#' @param object A fitted brms model object. Required.
#' @param data A data frame or data table passed to \code{fitted()}
#'   as the new data to be used for predictions. Required.
#' @param summarize A logical value, whether or not to
#'   calculate summaries of the posterior predictions.
#'   Defaults to \code{TRUE}.
#' @param posterior A logical value whether or not to
#'   save and return the posterior samples. Defaults
#'   to \code{FALSE} as the assumption is a typical
#'   use case is to return the summaries only.
#' @param dpar Parameter passed on the \code{dpar}
#'   argument of \code{fitted()} in brms.
#' @param re_formula Parameter passed on the \code{re_formula}
#'   argument of \code{fitted()} in brms.
#' @param resample An integer indicating the number of
#'   bootstrap resamples of the posterior predictions to
#'   use when calculating summaries. Defaults to \code{0L}.
#'   See the details section for more informations as its implementation
#'   is experimental and it may not operate as one would expect.
#' @param seed A seed for random number generation. Missing by default.
#'   Only needed if \code{resample} is a non zero integer.
#' @param ... Additional arguments passed to \code{fitted()}
#' @return A list with \code{Summary} and \code{Posterior}.
#'   Some of these may be \code{NULL} depending on the arguments used.
#' @keywords internal
#' @importFrom stats fitted
.predict <- function(object, data, summarize = TRUE, posterior = FALSE,
                     dpar = NULL, re_formula = NULL, resample = 0L, seed, ...) {
  out <- list(
    Summary = NULL,
    Posterior = NULL)

  out$Posterior <- fitted(
    object = object,
    newdata = data,
    re_formula = re_formula,
    scale = "response",
    dpar = dpar,
    summary = FALSE)

  if (isTRUE(resample == 0)) {
    out$Posterior <- rowMeans(out$Posterior, na.rm = TRUE)
  } else if (isTRUE(resample > 0)) {
    if (isFALSE(missingArg(seed))) {
      set.seed(seed)
    }

    yhat <- matrix(NA_real_, nrow = nrow(out$Posterior), ncol = resample)
    for (i in 1:resample) {
      yhat[, i] <- rowBootMeans(out$Posterior)
    }

    out$Posterior <- as.vector(yhat)
    rm(yhat)
  }

  if (isTRUE(summarize)) {
    out$Summary <- bsummary(out$Posterior, ...)
  }
  if (isFALSE(posterior)) {
    out$Posterior <- NULL
  }
  return(out)
}
