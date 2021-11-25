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
#' @param CIType A character string indicating the type of credible interval, passed on
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
bsummary <- function(x, CI = 0.99, CIType = "HDI", ROPE = NULL, MID = NULL) {
  ropes <- .percent(x, window = ROPE, within = TRUE)
  mids <- .percent(x, window = MID, within = FALSE)

  m <- mean(x, na.rm = TRUE)
  mdn <- median(x, na.rm = TRUE)
  cis <- bayestestR::ci(x, ci = CI, method = CIType)
  out <- data.table(
    M = m,
    Mdn = mdn,
    LL = cis$CI_low,
    UL = cis$CI_high,
    PercentROPE = ropes$Percent,
    PercentMID = mids$Percent,
    CI = CI,
    CIType = CIType,
    ROPE = ropes$Label,
    MID = mids$Label)

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

#' Function to check whether a brmsfit has random effects
#'
#' Internal utility function to check whether a \code{brmsfit}
#' object has any random effects, or not.
#'
#' @param object An object to be evaluated.
#' @return \code{TRUE} if any random effects present.
#'   \code{FALSE} if no random effects present.
#' @keywords internal
is.random <- function(object) {
  .assertbrmsfit(object)

  isTRUE(nrow(object$ranef) >= 1L)
}


#' Internal function to build out scale, inverselink and function
#'
#' Internal utility function used in \code{.predict()}
#'
#' @param link The link named in a \code{brmsfit} object
#' @param effects A character string, the type of effect desired
#' @param backtrans A character string, the type of back transformation
#' @return A list. TODO.
#' @keywords internal
.links <- function(link,
                   effects = c("fixedonly", "includeRE", "integrateoutRE"),
                   backtrans = c("response", "linear", "identity", "invlogit", "exp", "square")) {
  effects <- match.arg(effects, several.ok = FALSE)
  backtrans <- match.arg(backtrans, several.ok = FALSE)

  if (isTRUE(backtrans %in% c("linear", "identity"))) {
    ## options if back transformation is linear (meaning on linear scale)
    ## or identity, meaning no back transformation
    ## either way we do the same thing which is nothing
    scale <- "linear"
    inverselink <- "identity"
  } else if (isTRUE(backtrans %in% c("invlogit", "exp", "square"))) {
    ## options if back transformations were custom specified
    ## in these cases we get predictions on linear scale
    ## and then manually apply back transformation
    scale <- "linear"
    inverselink <- backtrans
  } else if (isTRUE(backtrans %in% "response")) {
    ## options for when using the generic asking for
    ## predictions on the original response scale
    ## in this case we need to proceed differently depending on
    ## the 'effect' argument
    if (isTRUE(effects %in% c("fixedonly", "includeRE"))) {
      ## for both these 'effects' we can rely on fitted()
      ## to apply the correct back transformation, so we
      ## do not need to do anything other than set scale to "response"
      scale <- "response"
      inverselink <- "identity"
    } else if (isTRUE(effects == "integrateoutRE")) {
      ## when integrating out random effects
      ## we cannot rely on backtransformation being handled
      ## by fitted(), so we must do it manually
      scale <- "linear"
      if (isTRUE(link == "identity")) {
        inverselink <- "identity"
      } else if (isTRUE(link == "logit")) {
        inverselink <- "invlogit"
      } else if (isTRUE(link == "log")) {
        inverselink <- "exp"
      } else if (isTRUE(link == "sqrt")) {
        inverselink <- "square"
      } else {
        stop("non supported link function detected for integrating out REs")
      }
    }
  }

  ## inverse link function
  inversefun <- switch(inverselink,
                       identity = function(x) x,
                       invlogit = plogis,
                       exp = exp,
                       square = function(x) x^2)

  ## argument for integratere() C++ function
  inverselinknum <- switch(inverselink,
                           identity = -9L,
                           invlogit = 0L,
                           exp = 1L,
                           square = 2L)

  ## when integrating out REs, need to use identity inversefun
  ## because the back transformation happens in C++ via inverselinknum
  if (isTRUE(effects == "integrateoutRE")) {
    inversefun <- function(x) x
  }

  list(
    scale = scale,
    ilink = inverselink,
    ifun = inversefun,
    ilinknum = inverselinknum)
}
