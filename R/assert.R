#' @title Internal functions to check assertions about a \code{brmsfit} model object
#'
#' @description
#' These are a set of internal utility functions.
#' They are not intended for general use.
#' Instead, they are intended to be called in circumstances
#' where the expected result is \code{TRUE}.
#' All of them are designed to try to give informative error
#' messages if the assertion is not met.
#' All of them result in a \code{stop()} error if the assertion is not met.
#'
#' @details
#' \itemize{
#'   \item{\code{.assertbrmsfit}}{asserts that the object should be of class \code{brmsfit}.}
#'   \item{\code{.assertgaussian}}{asserts that all random effects are Gaussian.}
#'   \item{\code{.assertfamily}}{asserts that the distribution (family) of the outcome is a currently supported family. Only applies when integrating out random effects.}
#'   \item{\code{.assertlink}}{asserts that the link function is a currently supported link function. Only applies when integrating out random effects.}
#' }
#'
#' @param object A \code{brmsfit} model object to be evaluated.
#'
#' @return An invisible, logical \code{TRUE} if the assertion is met.
#'   An (informative) error message if the assertion is not met.
#' @keywords internal
#' @name assertall
NULL


#' @rdname assertall
#' @importFrom brms is.brmsfit
.assertbrmsfit <- function(object) {
  if (!isTRUE(is.brmsfit(object))) {
    stop(sprintf("object must be of class 'brmsfit', but was %s",
                 paste(class(object), collapse = "; ")))
  } else {
    invisible(TRUE)
  }
}

#' @rdname assertall
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

#' @rdname assertall
.assertfamily <- function(object) {
  .assertbrmsfit(object)
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

#' @rdname assertall
.assertlink <- function(object) {
  .assertbrmsfit(object)
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
