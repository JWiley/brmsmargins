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


#' Function to check that something is a brmsfit object
#'
#' Internal utility function to check, with a useful error if not,
#' whether and object has class \code{brmsfit}.
#'
#' @param object An object to be evaluated.
#' @return An informative error if not of class \code{brmsfit},
#'   otherwise invisibly returns \code{TRUE}.
#' @keywords internal
#' @importFrom brms is.brmsfit
.assertbrmsfit <- function(object) {
  if (!isTRUE(is.brmsfit(object))) {
    stop(sprintf("object must be of class 'brmsfit', but was %s",
                 paste(class(object), collapse = "; ")))
  } else {
    invisible(TRUE)
  }
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

#' Function to check whether a brmsfit with random effects are all gaussian
#'
#' Internal utility function to check whether a \code{brmsfit}
#' object has all gaussian random effects.
#'
#' @param object An object to be evaluated.
#' @return \code{TRUE} if all random effects present are Gaussian or
#'   if there are no random effects.
#'   \code{FALSE} if any random effect present is not Gaussian.
#' @keywords internal
.assertgaussian <- function(object) {
  .assertbrmsfit(object)

  result <- FALSE
  if (isTRUE(is.random(object))) {
    if (isFALSE(all(object$ranef$dist == "gaussian"))) {
      err <- sprintf(paste0("Currently only gaussian random effects are supported, ",
                            "but the following distribution(s) were found %s."),
                     paste(unique(object$ranef$dist), collapse = "; "))
      stop(err)
    } else {
      result <- TRUE
    }
  } else {
    result <- TRUE
  }
  invisible(result)
}

#' Function to generate names of an L matrix from brms
#'
#' Internal utility function to check create the variable names for the
#' Cholesky decomposition of the random effects correlation matrix in \code{brms}.
#' \code{brms} returns the lower triangular matrix and we want the upper triangular
#' matrix, so the names are transposed. The results can then be passed to
#' the \code{tab2mat} function to convert the row vector into a matrix.
#'
#' @param block An integer, which random effect block object to be used.
#' @param ncol An integer, the total number of columns. Since this is a square matrix,
#'   this also defines how many rows.
#' @return A character string of the names.
#' @keywords internal
#' @importFrom data.table as.data.table
.namesL <- function(block, number) {
  n <- expand.grid(Block = block,
                   Row = seq_len(number),
                   Col = seq_len(number))
  n <- as.data.table(n)
  n[, sprintf("L_%d[%d,%d]",
              Block, Row, Col)]
}

.buildL <- function(data, block, number) {
  n <- .namesL(block, number)
  as.matrix(data[, n])
}

.namesSD <- function(ranef, block) {
  n <- subset(ranef, id == block)
  n <- as.data.table(n)
  n[, sprintf("sd_%s__%s", group, coef)]
}

.buildSD <- function(data, ranef, block) {
  n <- .namesSD(ranef, block)
  as.matrix(data[, n])
}

.namesZ <- function(block, number) {
  n <- expand.grid(Block = block,
                   Number = seq_len(number))
  n <- as.data.table(n)
  n[, sprintf("Z_%d_%d", Block, Number)]
}

.buildZ <- function(data, block, number) {
  n <- .namesZ(block, number)
  do.call(cbind, data[n])
}

.assertfamily <- function(object) {
  family <- object$family$family
  validlength <- identical(length(family), 1L)
  if (isFALSE(validlength)) {
    stop(sprintf("The 'family' must be a character string of length 1, but found length %d.",
                 length(family)))
  }

  validclass <- is.character(family)
  if (isFALSE(validclass)) {
    stop(sprintf("The 'family' must be a character string, but a %s class was found.",
                 paste(class(family), collapse = "; ")))
  }

  fams <- c("gaussian", "bernoulli", "poisson", "negbinomial")
  validtype <- family %in% fams
  if (isFALSE(validtype)) {
    stop(sprintf("The 'family' must be one of (%s), but found %s.",
                 paste(fams, collapse = ", "),
                 family))
  }
  invisible(TRUE)
}

.assertlink <- function(object) {
  link <- object$family$link
  validlength <- identical(length(link), 1L)
  if (isFALSE(validlength)) {
    stop(sprintf("The 'link' must be a character string of length 1, but found length %d.",
                 length(link)))
  }

  validclass <- is.character(link)
  if (isFALSE(validclass)) {
    stop(sprintf("The 'link' must be a character string, but a %s class was found.",
                 paste(class(link), collapse = "; ")))
  }

  linkfun <- c("identity", "logit", "log")
  validtype <- link %in% linkfun
  if (isFALSE(validtype)) {
    stop(sprintf("The 'link' must be one of (%s), but found %s.",
                 paste(linkfun, collapse = ", "),
                 link))
  }
  invisible(TRUE)
}







## ## R code versions of C++ functions for testing and comparison purposes
## integratemvnR <- function(X, k, sd, chol) {
##   n <- length(sd)
##   Z <- matrix(rnorm(k * n, mean = 0, sd = 1), nrow = k, ncol = n)
##   if (n > 1) {
##     Z <- Z %*% chol
##   }
##   for (i in seq_len(n)) {
##     Z[, i] <- Z[, i] * sd[i]
##   }
##   X %*% t(Z)
## }
## tab2matR <- function(X) {
##   X <- as.vector(X)
##   dims <- sqrt(length(X))
##   matrix(X, dims, dims, byrow = TRUE)
## }
## integratereR <- function(d, sd, L, k, yhat, backtrans) {
##   M <- nrow(yhat)
##   N <- ncol(yhat)
##   J <- length(sd)
##   yhat2 <- matrix(0, M, N)
##   for (i in seq_len(M)) {
##     Z <- vector("list", J)
##     for (re in seq_len(J)) {
##       cholmat <- tab2matR(L[[re]][i, ])
##       dmat <- d[[re]]
##       Z[[re]] <- integratemvnR(dmat, k, sd[[re]][i, ], cholmat)
##     }
##     Zall <- Z[[1]]
##     if (J > 1) {
##       for (re in 2:J) {
##         Zall <- Zall + Z[[re]]
##       }
##     }
##     for (nsamp in seq_len(k)) {
##       Zall[, nsamp] <- Zall[, nsamp] + t(yhat[i, ])
##     }
##     if (backtrans == 0) {
##       Zall <- 1 / (1 + exp(-Zall))
##     } else if (backtrans == 1) {
##       Zall <- exp(Zall)
##     } else if (backtrans == 2) {
##       Zall <- Zall^2
##     }
##     zm <- rowMeans(Zall)
##     yhat2[i, ] <- t(zm)
##   }
##   return(yhat2)
## }
