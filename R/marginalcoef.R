#' Marginal Coefficients from a 'brms' Model
#'
#' Calculate marginal coefficients from a `brms`
#' generalized linear mixed model using the method proposed by Hedeker (2018).
#'
#' @param object A fitted `brms` model object that includes random effects. Required.
#' @param summarize A logical value, whether or not to
#'   calculate summaries of the posterior predictions.
#'   Defaults to `TRUE`.
#' @param posterior A logical value whether or not to
#'   save and return the posterior samples. Defaults
#'   to `FALSE` as the assumption is a typical
#'   use case is to return the summaries only.
#' @param index An optional integer vector, giving the posterior draws
#'   to be used in the calculations. If omitted, defaults to all
#'   posterior draws.
#' @param backtrans A character string indicating the type of
#'   back transformation to be applied. Can be one of
#'   "response" meaning to use the response scale,
#'   "linear" or "identity" meaning to use the linear predictor scale,
#'   or a specific back transformation desired, from a possible list of
#'   "invlogit", "exp", "square", or "inverse".
#'   Custom back transformations should only be needed if, for example,
#'   the outcome variable was transformed prior to fitting the model.
#' @param k An integer providing the number of random draws to use for
#'   integrating out the random effects. Only relevant when `effects = "integrateoutRE"`.
#' @param seed An *optional* argument that controls whether (and if so what) random seed
#'   to use. This can help with reproducibility of results.
#'   It is missing by default.
#' @param ... Additional arguments passed to [bsummary()], 
#'   and only relevant if `summarize` is `TRUE`.
#' @return A list with `Summary` and `Posterior`.
#'   Some of these may be `NULL` depending on the arguments used.
#' @references
#' Hedeker, D., du Toit, S. H., Demirtas, H. & Gibbons, R. D. (2018)
#' \doi{10.1111/biom.12707}.
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
                         k = 100L, seed, ...) {
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

  if (isFALSE(missing(seed))) {
    if (isFALSE(is.null(seed))) {
      stopifnot(identical(length(seed), 1L))
      set.seed(seed)
    }
  }
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
