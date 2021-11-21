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
#'   argument of \code{fitted()} in brms. Defaults to \code{NA}
#'   meaning that no random effects should be used.
#'   Irrelevant in models without any random effects.
#' @param resample An integer indicating the number of
#'   bootstrap resamples of the posterior predictions to
#'   use when calculating summaries. Defaults to \code{0L}.
#'   See documentation from [.averagePosterior()] for more details.
#' @param seed A seed for random number generation. Defaults to \code{FALSE},
#'   which means no seed is set.
#'   Only used if \code{resample} is a positive, non-zero integer.
#'   See documentation from [.averagePosterior()] for more details.
#' @param ... Additional arguments passed to \code{fitted()}
#' @return A list with \code{Summary} and \code{Posterior}.
#'   Some of these may be \code{NULL} depending on the arguments used.
#' @keywords internal
#' @importFrom data.table as.data.table
#' @importFrom stats fitted formula
#' @importFrom posterior as_draws_df ndraws
#' @importFrom brms make_standata
## object <- JWileymisc::readRDSfst("../mixedlogit.RDS")
## data <- model.frame(object)[1:2, ]
## data$x <- c(0, 1)
## system.time(test <- .predict(object = object, data = data, k = 1000L, index = 1:4000))
.predict <- function(object, data, summarize = TRUE, posterior = FALSE,
                     index, dpar = NULL, resample = 0L, seed = FALSE,
                     integrateoutRE = is.random(object),
                     backtrans = NULL, k = 100L, re_formula = NA, ...) {
  .assertbrmsfit(object)

  if (isTRUE(missingArg(index))) {
    index <- seq_len(ndraws(object))
  }

  if (isFALSE(integrateoutRE)) {
  posterior <- fitted(
    object = object, newdata = data,
    re_formula = re_formula, scale = "response", dpar = dpar,
    draw_ids = index, summary = FALSE)
  }

  if (isTRUE(integrateoutRE)) {
    ## this function only for random effect models
    if (isFALSE(is.random(object))) {
      stop("integrateoutRE can only be TRUE for models with random effects")
    }

    ## assert the assumed family / distribution is a supported one
    .assertfamily(object)
    ## assert the link function used is a supported one
    .assertlink(object)
    ## assert that all random effects in the model are Gaussian
    .assertgaussian(object)

    ## back transformation
    if (is.null(backtrans)) {
      if (object$family$link == "identity") {
        backtrans <- "identity"
      }
      if (object$family$link == "logit") {
        backtrans <- "invlogit"
      }
      if (object$family$link == "log") {
        backtrans <- "exp"
      }
      ## if (object$family$link == "sqrt") {
      ##   backtrans <- "square"
      ## }
    }
    stopifnot(backtrans %in% c("identity", "invlogit", "exp", "square"))

    backtransnum <- switch(backtrans,
                           identity = NA_integer_,
                           invlogit = 0L,
                           exp = 1L,
                           square = 2L)

    posterior <- fitted(
      object = object, newdata = data,
      re_formula = NA, scale = "linear", dpar = dpar,
      draw_ids = index, summary = FALSE)

    if (isTRUE(backtrans != "identity")) {
      post <- as.data.table(posterior::as_draws_df(object))[index, ]

      dtmp <- make_standata(formula(object), data = data)

      re <- as.data.table(object$ranef)

      if (is.null(dpar)) {
        usedpar <- ""
      }

      re <- re[dpar == usedpar]

      blocks <- unique(re$id)
      nblocks <- length(blocks)

      d2 <- sd <- L <- vector("list", nblocks)

      for (i in seq_len(nblocks)) {
        useblock <- blocks[i]
        usere <- re[id == useblock]
        num <- max(usere$cn)
        d2[[i]] <- .buildZ(data = dtmp, block = useblock, number = num)
        sd[[i]] <- .buildSD(data = post, ranef = usere, block = useblock)
        L[[i]] <- .buildL(data = post, block = useblock, number = num)
        names(d2)[i] <- names(sd)[i] <- names(L)[i] <- sprintf("Block%d", useblock)
      }

      posterior <- integratere(d = d2, sd = sd, L = L, k = k,
                               yhat = posterior, backtrans = backtransnum)
    }
  }

  posterior <- averagePosterior(posterior, resample = resample, seed = seed)

  out <- list(
    Summary = NULL,
    Posterior = NULL)

  if (isTRUE(summarize)) {
    out$Summary <- bsummary(posterior, ...)
  }
  if (isTRUE(posterior)) {
    out$Posterior <- posterior
  }
  return(out)
}
