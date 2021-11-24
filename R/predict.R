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
#' @param index An optional integer vector, giving the posterior draws
#'   to be used in the calculations. If omitted, defaults to all
#'   posterior draws.
#' @param dpar Parameter passed on the \code{dpar}
#'   argument of \code{fitted()} in brms. Defaults to \code{NULL}
#'   indicating the mean or location parameter typically.
#' @param resample An integer indicating the number of
#'   bootstrap resamples of the posterior predictions to
#'   use when calculating summaries. Defaults to \code{0L}.
#'   See documentation from [.averagePosterior()] for more details.
#' @param seed A seed for random number generation. Defaults to \code{FALSE},
#'   which means no seed is set.
#'   Only used if \code{resample} is a positive, non-zero integer.
#'   See documentation from [.averagePosterior()] for more details.
#' @param effects A character string indicating the type of
#'   prediction to be made. Can be one of
#'   \dQuote{fixedonly} meaning only use fixed effects,
#'   \dQuote{includeRE} meaning that random effects should be
#'   included in the predictions, or
#'   \dQuote{integrateoutRE} meaning that random effects should be
#'    integrated out / over in the predictions.
#' @param backtrans A character string indicating the type of
#'   back transformation to be applied. Can be one of
#'   \dQuote{response} meaning to use the response scale,
#'   \dQuote{linear} or \dQuote{identity} meaning to use the linear predictor scale,
#'   or a specific back transformation desired, from a possible list of
#'   \dQuote{invlogit}, \dQuote{exp}, or \dQuote{square}.
#'   Custom back transformations should only be needed if, for example,
#'   the outcome variable was transformed prior to fitting the model.
#' @param k An integer providing the number of random draws to use for
#'   integrating out the random effects. Only relevant when \code{effects}
#'   is \dQuote{integrateoutRE}.
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
## system.time(test0 <- .predict(object = object, data = data[1, ], posterior = TRUE,
##                              effects = "integrateoutRE", k = 1000L, index = 1:4000))
## system.time(test1 <- .predict(object = object, data = data[2, ], posterior = TRUE,
##                              effects = "integrateoutRE", k = 1000L, index = 1:4000))
## system.time(test0f <- .predict(object = object, data = data[1, ], posterior = TRUE,
##                              effects = "fixedonly", k = 1000L, index = 1:4000))
## system.time(test1f <- .predict(object = object, data = data[2, ], posterior = TRUE,
##                              effects = "fixedonly", k = 1000L, index = 1:4000))
## system.time(test0re <- .predict(object = object, data = data[1, ], posterior = TRUE,
##                              effects = "includeRE", k = 1000L, index = 1:4000))
## system.time(test1re <- .predict(object = object, data = data[2, ], posterior = TRUE,
##                              effects = "includeRE", k = 1000L, index = 1:4000))
.predict <- function(object, data, summarize = TRUE, posterior = FALSE,
                     index, dpar = NULL, resample = 0L, seed = FALSE,
                     effects = c("fixedonly", "includeRE", "integrateoutRE"),
                     backtrans = c("response", "linear", "identity", "invlogit", "exp", "square"),
                     k = 100L, ...) {
  ## checks and assertions
  .assertbrmsfit(object)

  if (isFALSE(is.random(object))) {
    if (isFALSE(effects == "fixedonly")) {
      stop("object does not have random effects: must use \"effects = 'fixedonly'\"")
    }
  }

  effects <- match.arg(effects, several.ok = FALSE)
  backtrans <- match.arg(backtrans, several.ok = FALSE)

  if (isTRUE(effects == "integrateoutRE")) {
    ## assert the assumed family / distribution is a supported one
    .assertfamily(object)
    ## assert the link function used is a supported one
    .assertlink(object)
    ## assert that all random effects in the model are Gaussian
    .assertgaussian(object)
  }

  if (isTRUE(missingArg(index))) {
    index <- seq_len(ndraws(object))
  }

  links <- .links(link = object$family$link,
    effects = effects, backtrans = backtrans)

  ## set whether fitted() should include RE (NULL) or not (NA)
  ## see help for ?fitted.brmsfit for more details
  if (isTRUE(effects %in% c("fixedonly", "integrateoutRE"))) {
    useRE <- NA
  } else if (isTRUE(effects == "includeRE")) {
    useRE <- NULL
  }

  ## generate all predictions (if fixedonly or includeRE)
  ## or generate just the fixed effects predictions (if integrateRE)
  posterior <- fitted(
    object = object, newdata = data,
    re_formula = useRE,
    scale = links$scale, dpar = dpar,
    draw_ids = index, summary = FALSE)
  posterior <- links$ifun(posterior)

  if (isTRUE(effects == "integrateoutRE")) {
    if (isTRUE(links$ilink != "identity")) {
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
                               yhat = posterior, backtrans = links$ilinknum)
    }
  }

  ## average across rows
  ## either using row wise means, or row wise bootstrapped means
  posterior <- .averagePosterior(posterior, resample = resample, seed = seed)

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
