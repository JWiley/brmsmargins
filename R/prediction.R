#' Marginal Posterior Predictions from a 'brms' Model
#'
#' Calculate marginal predictions from a \code{brms} model.
#' Marginal predictions average over the input data for each posterior draw.
#' Marginal predictions for models with random effects will integrate
#' over random effects.
#' Arguments are labeled as \emph{required} when it is required that the
#' user directly specify the argument. Arguments are labeled as
#' \emph{optional} when either the argument is optional or there are
#' sensible default values so that users do not typically need to specify
#' the argument.
#'
#' @param object A \emph{required} argument specifying a fitted
#'   \code{brms} model object.
#' @param data A \emph{required} argument specifying a data frame or
#'   data table passed to \code{fitted()} as the new data to be used
#'   for predictions.
#' @param summarize An \emph{optional} argument, a logical value, whether
#'   or not to calculate summaries of the posterior predictions.
#'   Defaults to \code{TRUE}.
#' @param posterior An \emph{optional} argument, a logical value whether
#'   or not to save and return the posterior samples. Defaults
#'   to \code{FALSE} as the assumption is a typical
#'   use case is to return the summaries only.
#' @param index An \emph{optional} argument, an integer vector, giving the
#'   posterior draws to be used in the calculations. If omitted,
#'   defaults to all posterior draws.
#' @param dpar An \emph{optional} argument, the parameter passed on to the
#'   \code{dpar} argument of \code{fitted()} in brms. Defaults to \code{NULL}
#'   indicating the mean or location parameter typically.
#' @param resample An \emph{optional} argument, an integer indicating the
#'   number of bootstrap resamples of the posterior predictions to
#'   use when calculating summaries. Defaults to \code{0L}.
#'   See documentation from [.averagePosterior()] for more details.
#'   This should be considered experimental.
#' @param resampleseed An \emph{optional} argument, a seed for random number
#'   generation. Defaults to \code{FALSE}, which means no seed is set.
#'   Only used if \code{resample} is a positive, non-zero integer.
#'   See documentation from [.averagePosterior()] for more details.
#'   This should be considered experimental.
#' @param effects An \emph{optional} argument, a character string indicating
#'   the type of prediction to be made. Can be one of
#'   \dQuote{fixedonly} meaning only use fixed effects,
#'   \dQuote{includeRE} meaning that random effects should be
#'   included in the predictions, or
#'   \dQuote{integrateoutRE} meaning that random effects should be
#'    integrated out / over in the predictions.
#'   It defaults to \dQuote{fixedonly} so is not typically required for
#'   a user to specify it.
#' @param backtrans An \emph{optional} argument, a character string indicating
#'   the type of back transformation to be applied. Can be one of
#'   \dQuote{response} meaning to use the response scale,
#'   \dQuote{linear} or \dQuote{identity} meaning to use the linear predictor scale,
#'   or a specific back transformation desired, from a possible list of
#'   \dQuote{invlogit}, \dQuote{exp}, \dQuote{square}, or \dQuote{inverse}.
#'   Custom back transformations should only be needed if, for example,
#'   the outcome variable was transformed prior to fitting the model.
#'   It defaults to \dQuote{response} so is not typically required for
#'   a user to specify it.
#' @param k An \emph{optional} argument, an integer providing the number of
#'   random draws to use for integrating out the random effects.
#'   Only relevant when \code{effects} is \dQuote{integrateoutRE}.
#'   It defaults to \code{100L}, a rather arbitrary number attempting to
#'   balance the increased precision that comes from a larger value,
#'   with the increased computational cost of more Monte Carlo simulations
#'   when integrating out random effects.
#' @param raw An \emph{optional} argument, a logical value indicating whether to
#'   return the raw output or to average over the Monte Carlo samples.
#'   Defaults to \code{FALSE}.
#'   Setting it to \code{TRUE} can be useful if you want not only the
#'   full posterior distribution but also the \code{k} Monte Carlo samples
#'   used for the numerical integration. This cannot be used with
#'   \code{summarize = TRUE}.
#' @param ... Additional arguments passed to \code{bsummary()},
#'   and only relevant if \code{summarize} is \code{TRUE}.
#' @return A list with \code{Summary} and \code{Posterior}.
#'   Some of these may be \code{NULL} depending on the arguments used.
#' @references
#' Pavlou, M., Ambler, G., Seaman, S., & Omar, R. Z. (2015)
#' \doi{10.1186/s12874-015-0046-6}
#' \dQuote{A note on obtaining correct marginal predictions from a random intercepts model for binary outcomes}
#' and
#' Skrondal, A., & Rabe-Hesketh, S. (2009)
#' \doi{10.1111/j.1467-985X.2009.00587.x}
#' \dQuote{Prediction in multilevel generalized linear models}
#' @importFrom data.table as.data.table
#' @importFrom stats fitted formula
#' @importFrom posterior as_draws_df ndraws
#' @importFrom brms standata
#' @export
prediction <- function(object, data, summarize = TRUE, posterior = FALSE,
                       index, dpar = NULL, resample = 0L, resampleseed = FALSE,
                       effects = c("fixedonly", "includeRE", "integrateoutRE"),
                       backtrans = c("response", "linear", "identity",
                                     "invlogit", "exp", "square", "inverse"),
                       k = 100L, raw = FALSE, ...) {
  ## checks and assertions
  if (isTRUE(missing(object))) {
    stop(paste(
      "'object' is a required argument and cannot be missing;",
      "  it should be a saved model fit from brms. For example:",
      "  m <- brm(y ~ x, data = yourdata)",
      "  See ?prediction or the website articles (vignettes) for details.",
      "  https://joshuawiley.com/brmsmargins/", sep = "\n"))
  }
  .assertbrmsfit(object)
  .assertdpar(object, dpar = dpar)

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
    .assertlink(object, dpar = dpar)
    ## assert that all random effects in the model are Gaussian
    .assertgaussian(object)
  }

  if (isTRUE(missing(index))) {
    index <- seq_len(ndraws(object))
  }

  links <- .links(
    link = .extractlink(object, dpar),
    effects = effects, backtrans = backtrans)

  ## set whether fitted() should include RE (NULL) or not (NA)
  ## see help for ?fitted.brmsfit for more details
  if (isTRUE(effects %in% c("fixedonly", "integrateoutRE"))) {
    useRE <- NA
  } else if (isTRUE(effects == "includeRE")) {
    useRE <- NULL
  }

  ## generate all predictions (if fixedonly or includeRE)
  ## or generate just the fixed effects predictions (if integrateoutRE)
  yhat <- fitted(
    object = object, newdata = data,
    re_formula = useRE,
    scale = links$scale, dpar = dpar,
    draw_ids = index, summary = FALSE)
  yhat <- links$useifun(yhat)

  if (isTRUE(effects == "integrateoutRE")) {
    if (isTRUE(links$ilink != "identity")) {
      post <- as.data.table(as_draws_df(object))[index, ]

      dtmp <- standata(object, newdata = data, check_response = FALSE, allow_new_levels = TRUE)

      re <- as.data.table(object$ranef)

      if (is.null(dpar)) {
        usedpar <- ""
      } else {
        usedpar <- dpar
      }

      re <- re[dpar == usedpar]

      blocks <- unique(re$id)
      nblocks <- length(blocks)

      d2 <- sd <- L <- vector("list", nblocks)

      for (i in seq_len(nblocks)) {
        useblock <- blocks[i]
        usere <- re[id == useblock]
        num <- max(usere$cn)
        d2[[i]] <- .buildZ(data = dtmp, block = useblock, number = num, dpar = dpar)
        sd[[i]] <- .buildSD(data = post, ranef = usere, block = useblock, dpar = dpar)
        L[[i]] <- .buildL(data = post, block = useblock, number = num)
        names(d2)[i] <- names(sd)[i] <- names(L)[i] <- sprintf("Block%d", useblock)
      }

      yhat <- integratere(d = d2, sd = sd, L = L, k = k,
                          yhat = yhat, backtrans = links$useilinknum)
    }
  }

  if (isTRUE(raw)) {
    if (isTRUE(summarize)) {
      message("summarize cannot be TRUE when raw = TRUE, setting to FALSE")
      summarize <- FALSE
    }
    if (isFALSE(posterior)) {
      message("posterior cannot be FALSE when raw = TRUE, setting to TRUE")
      posterior <- TRUE
    }
  } else {
    ## average across rows
    ## either using row wise means, or row wise bootstrapped means
    yhat <- .averagePosterior(yhat, resample = resample, seed = resampleseed)
  }

  out <- list(
    Summary = NULL,
    Posterior = NULL)

  if (isTRUE(summarize)) {
    out$Summary <- bsummary(yhat, ...)
  }
  if (isTRUE(posterior)) {
    out$Posterior <- yhat
  }

  return(out)
}
