#' Marginal Coefficients from a 'brms' Model
#'
#' Calculate marginal coefficients from a \code{brms}
#' generalized linear mixed model using the method proposed by Hedeker (2018).
#'
#' @param object A fitted brms model object that includes random effects. Required.
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
#' @param backtrans A character string indicating the type of
#'   back transformation to be applied. Can be one of
#'   \dQuote{response} meaning to use the response scale,
#'   \dQuote{linear} or \dQuote{identity} meaning to use the linear predictor scale,
#'   or a specific back transformation desired, from a possible list of
#'   \dQuote{invlogit}, \dQuote{exp}, \dQuote{square}, or \dQuote{inverse}.
#'   Custom back transformations should only be needed if, for example,
#'   the outcome variable was transformed prior to fitting the model.
#' @param k An integer providing the number of random draws to use for
#'   integrating out the random effects. Only relevant when \code{effects}
#'   is \dQuote{integrateoutRE}.
#' @param ... Additional arguments passed to \code{fitted()}
#' @return A list with \code{Summary} and \code{Posterior}.
#'   Some of these may be \code{NULL} depending on the arguments used.
#' @references
#' Hedeker, D., du Toit, S. H., Demirtas, H. & Gibbons, R. D. (2018)
#' \doi{10.1111/biom.12707}
#' \dQuote{A note on marginalization of regression parameters from mixed models of binary outcomes}
#' @importFrom data.table as.data.table
#' @importFrom stats formula
#' @importFrom posterior as_draws_df ndraws
#' @importFrom brms make_standata
#' @importFrom methods missingArg
#' @export
marginalcoef <- function(object, summarize = TRUE, posterior = FALSE, index,
                         backtrans = c("response", "linear", "identity",
                                       "invlogit", "exp", "square", "inverse"),
                         k = 100L, ...) {
  ## checks and assertions
  .assertbrmsfit(object)

  if (isFALSE(is.random(object))) {
    stop("object must have random effects to use marginalcoef()")
  }

  ## assert the assumed family / distribution is a supported one
  .assertfamily(object)
  ## assert the link function used is a supported one
  .assertlink(object)
  ## assert that all random effects in the model are Gaussian
  .assertgaussian(object)

  if (isTRUE(missingArg(index))) {
    index <- seq_len(ndraws(object))
  }

  links <- .links(
    link = .extractlink(object, NULL),
    effects = "integrateoutRE", backtrans = backtrans)

  mf <- model.frame(object)
  X <- make_standata(formula(object), data = mf)$X

  lambda <- prediction(
    object, data = mf,
    summarize = FALSE, posterior = TRUE, index = index,
    effects = "integrateoutRE", backtrans = backtrans,
    k = k, raw = TRUE)

  y <- links$fun(t(lambda$Posterior))

  B <- lmcpp(X, y)

  out <- list(
    Summary = NULL,
    Posterior = NULL)

  if (isTRUE(summarize)) {
    out$Summary <- as.data.table(do.call(rbind, apply(B, 1, bsummary, ...)))
    out$Summary[, Label := colnames(X)]
  }
  if (isTRUE(posterior)) {
    out$Posterior <- B
  }

  return(out)
}
