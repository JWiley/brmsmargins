#' Function to generate posterior predictions and summaries from a brms model with random effects
#'
#' This is an early developmental function.
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
#' @importFrom stats fitted formula
#' @importFrom posterior as_draws_df
#' @importFrom brms make_standata
## object <- JWileymisc::readRDSfst("../mixedlogit.RDS")
## data <- model.frame(object)[1:2, ]
## data$x <- c(0, 1)
## test <- .predictre(object = object, data = data, index = 1:5)
.predictre <- function(object, data, backtrans = NULL, index,
                       summarize = TRUE, posterior = FALSE,
                       dpar = NULL, re_formula = NULL, k = 100L, resample = 0L, seed, ...) {
  ## assert that the model object is a brmsfit object
  .assertbrmsfit(object)
  ## assert the assumed family / distribution is a supported one
  .assertfamily(object)
  ## assert the link function used is a supported one
  .assertlink(object)
  ## this function only for random effect models
  if (isFALSE(is.random(object))) {
    stop(".predictre is only for models with random effects")
  }
  ## assert that all random effects in the model are Gaussian
  .assertgaussian(object)

  ## back transformation
  if (is.null(backtrans)) {
    if (object$family$link == "identity") {
      ## do something
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

  post <- posterior::as_draws_df(object)[index, , drop = FALSE]

  dtmp <- make_standata(formula(object), data = data)

  re <- object$ranef

  if (is.null(dpar)) {
    usedpar <- ""
  }

  re <- subset(re, dpar == usedpar)

  blocks <- unique(re$id)
  nblocks <- length(blocks)

  d2 <- sd <- L <- vector("list", nblocks)

  for (i in seq_len(nblocks)) {
    useblock <- blocks[i]
    usere <- subset(re, id == useblock)
    num <- max(usere$cn)
    d2[[i]] <- .buildZ(data = dtmp, block = useblock, number = num)
    sd[[i]] <- .buildSD(data = post, ranef = usere, block = useblock)
    L[[i]] <- .buildL(data = post, block = useblock, number = num)
    names(d2)[i] <- names(sd)[i] <- names(L)[i] <- sprintf("Block%d", useblock)
  }

  yhat.fixed <- fitted(
    object = object, newdata = data,
    re_formula = NA, scale = "linear", dpar = dpar,
    draw_ids = index, summary = FALSE)

  out <- integratere(d = d2, sd = sd, L = L, k = k,
                     yhat = yhat.fixed, backtrans = backtransnum)

  ## out <- list(
  ##   Summary = NULL,
  ##   Posterior = NULL)
  ## out$Posterior <- fitted(
  ##   object = object,
  ##   newdata = data,
  ##   re_formula = re_formula,
  ##   scale = "response",
  ##   dpar = dpar,
  ##   summary = FALSE)
  ## if (isTRUE(resample == 0)) {
  ##   out$Posterior <- rowMeans(out$Posterior, na.rm = TRUE)
  ## } else if (isTRUE(resample > 0)) {
  ##   if (isFALSE(missingArg(seed))) {
  ##     set.seed(seed)
  ##   }
  ##   yhat <- matrix(NA_real_, nrow = nrow(out$Posterior), ncol = resample)
  ##   for (i in seq_len(resample)) {
  ##     yhat[, i] <- rowBootMeans(out$Posterior)
  ##   }
  ##   out$Posterior <- as.vector(yhat)
  ##   rm(yhat)
  ## }
  ## if (isTRUE(summarize)) {
  ##   out$Summary <- bsummary(out$Posterior, ...)
  ## }
  ## if (isFALSE(posterior)) {
  ##   out$Posterior <- NULL
  ## }

  return(out)
}
