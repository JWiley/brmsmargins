#' Calculate the percent of observations within or without a window
#'
#' This is an internal helper function to calculate and label
#' the percentage of a posterior distribution that falls within
#' the Region of Practical Equivalence (ROPE) or
#' at or beyond a Minimally Important Difference (MID).
#' It is designed to fail gracefully if no window given, and to
#' give some useful labels about the windows / range used.
#' Intended for use internally as part of \code{\link{brmsmargins}}.
#'
#' @param x A vector of values to evaluate. Required.
#' @param window An optional numeric vector giving a window.
#' @param within A logical value indicating whether to calculate the
#'   percentage within the window (if \code{TRUE}) or the
#'   percentage at or outside the window (if \code{FALSE}).
#'   Defaults to \code{TRUE}.
#' @return A list with the \code{Window}, if specified else \code{NULL},
#'   the \code{Percent} of observations, and a \code{Label} specifying the
#'   exact window used in human readable format.
#' @keywords internal
#' @importFrom extraoperators %e%
#' @examples
#' brmsmargins:::.percent(1:10, window = NULL)
#' brmsmargins:::.percent(1:10, window = c(3, 5))
#' brmsmargins:::.percent(1:10, window = c(2, 6), within = FALSE)
.percent <- function(x, window = NULL, within = TRUE) {
  if (isTRUE(is.null(window))) {
    window <- NA_real_
    pi <- NA_real_
    lab <- NA_character_
  } else {
    stopifnot(isTRUE(is.numeric(window)) && identical(length(window), 2L))
    if (isTRUE(within)) {
      lab <- sprintf("[%s, %s]",
                     as.character(min(window)),
                     as.character(max(window)))
    } else if (isFALSE(within)) {
      lab <- sprintf("[-Inf, %s] | [%s, Inf]",
                     as.character(min(window)),
                     as.character(max(window)))      
    }
    pi <- mean(x %e% lab, na.rm = TRUE) * 100
  }
  list(
    Window = window,
    Percent = pi,
    Label = lab)  
}

#' A Personal Preference Based Bayesian Summary Function
#'
#' This function returns a summary of a posterior distribution for a single
#' parameter / value. It is based on personal preference. Notably, it does not
#' only use \code{bayestestR::describe_posterior}, an excellent function,
#' because of the desire to also describe the percentage of the full posterior
#' distribution that is at or exceeding the value of a
#' Minimally Important Difference (MID). MIDs are used in clinical studies with outcome
#' measures where there are pre-defined differences that are considered clinically
#' important, which is distinct from the ROPE or general credible intervals capturing
#' uncertainty.
#'
#' @param x The posterior distribution of a parameter
#' @param CI A numeric value indicating the desired width of the credible interval.
#'   Defaults to \code{0.99} currently, but this is subject to change.
#'   a 99% interval was chosen as the default as there have been recent arguments
#'   made in the realm of meta science that there are, essentially, too many
#'   false positives and that many of the \dQuote{findings} in science are not able
#'   to be replicated.
#'   In any case, users should ideally specify a desired CI width, and not rely on
#'   defaults.
#' @param type A character string indicating the type of credible interval, passed on
#'   to the \code{\link[bayestestR]{ci}} function as the method for CIs.
#' @param ROPE Either left as \code{NULL}, the default, or a numeric vector of
#'   length 2, specifying the lower and upper thresholds for the
#'   Region of Practical Equivalence (ROPE).
#' @param MID Either left as \code{NULL}, the default, or a numeric vector of
#'   length 2, specifying the lower and upper thresholds for a 
#'   Minimally Important Difference (MID). Unlike the ROPE, percentages for
#'   the MID are calculated as at or exceeding the bounds specified by this
#'   argument, whereas the ROPE is the percentage of the posterior at or inside
#'   the bounds specified.
#' @return A \code{data.table} with the mean, \code{M}
#' \describe{
#'   \item{M}{the mean of the posterior samples}
#'   \item{Mdn}{the median of the posterior samples}
#'   \item{LL}{the lower limit of the credible interval}
#'   \item{UL}{the upper limit of the credible interval}
#'   \item{PercentROPE}{the percentage of posterior samples falling into the ROPE}
#'   \item{PercentMID}{the percentage of posterior samples falling at or beyond the MID}
#'   \item{CI}{the width of the credible interval used}
#'   \item{CIType}{the type of credible interval used (e.g., highest density interval)}
#'   \item{ROPE}{a label describing the values included in the ROPE}
#'   \item{MID}{a label describing the values included in the MID}
#' }
#' @export
#' @importFrom bayestestR ci
#' @importFrom data.table data.table
#' @importFrom stats median
#' @examples
#'
#' bsummary(rnorm(10000))
#' 
#' bsummary(rnorm(10000), ROPE = c(-.5, .5), MID = c(-1, 1))
bsummary <- function(x, CI = 0.99, type = "HDI", ROPE = NULL, MID = NULL) {
  ropes <- .percent(x, window = ROPE, within = TRUE)
  mids <- .percent(x, window = MID, within = FALSE)  
  
  m <- mean(x, na.rm = TRUE)
  mdn <- median(x, na.rm = TRUE)
  cis <- bayestestR::ci(x, ci = CI, method = type)
  out <- data.table(
    M = m,
    Mdn = mdn,
    LL = cis$CI_low,
    UL = cis$CI_high,
    PercentROPE = ropes$Percent,
    PercentMID = mids$Percent,
    CI = CI,
    CIType = type,
    ROPE = ropes$Label,
    MID = mids$Label)
    
  return(out)
}

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
.predict <- function(object, data, summarize = TRUE, posterior = FALSE, dpar = NULL, re_formula = NULL, resample = 0L, seed, ...) {
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

#' Function to check that something is a valid data object
#'
#' Internal utility function to check requirements for
#'   \code{\link{brmsmargins}}.
#'
#' @param x An object to be evaluated.
#' @param requireNames A logical, whether names are
#'   required. Defaults to \code{TRUE}
#' @return An empty string if no issues. Otherwise, a non zero
#'   string with warning/error messages.
#' @keywords internal
#' @importFrom data.table is.data.table
.checktab <- function(x, requireNames = TRUE) {
  xclass <- paste(class(x), collapse = "; ")
  pass1 <- isTRUE(inherits(x, "tbl")) ||
    isTRUE(is.data.frame(x)) ||
    isTRUE(is.data.table(x)) ||
    is.matrix(x)

  cnames <- colnames(x)
  pass2 <- isFALSE(is.null(cnames))

  errmsg1 <- errmsg2 <- ""
  
  if (isFALSE(pass1)) {
    errmsg1 <- sprintf(paste0(
      "Object is of class %s ",
      "but must be a matrix, data.frame, data.table, or tbl.\n"),
      xclass)
  }
  if (isFALSE(pass2)) {
    errmsg2 <- "Variables/Columns must be named, but column names were NULL.\n"
  }

  if (isTRUE(requireNames)) {
    out <- paste0(errmsg1, errmsg2)
  } else {
    out <- errmsg1
  }
  return(out)
}
