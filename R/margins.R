#' Calculate Average Marginal Effects (AMEs) from brms models
#'
#' This function is designed to help calculate average marginal
#' effects (AMEs) from brms models.
#' Currently, only one of \code{at} or \code{add} can be specified.
#' It is hoped to relax this in the future.
#' TODO add more documentation and technical definitions.
#'
#' @param object A fitted brms model object. Required.
#' @param at An optional object inheriting from data frame indicating
#'   the values to hold specific variables at when calculating average
#'   predictions. This is intended for AMEs from categorical variables.
#' @param add An optional object inheriting from data frame indicating
#'   the values to add to specific variables at when calculating average
#'   predictions. This is intended for AMEs for continuous variables.
#' @param newdata An object inheriting from data frame indicating
#'   the baseline values to use for predictions and AMEs.
#'   Defaults to be the model frame.
#' @param CI A numeric value specifying the width of the credible interval.
#'   Defaults to \code{0.99}.
#' @param CIType A character string specifying the type of credible interval
#'   (e.g., highest density interval). It is passed down to
#'   \code{\link{bsummary}} which in turn passes it to
#'   \code{\link[bayestestR]{ci}}. Defaults to \dQuote{HDI}.
#' @param contrasts An optional contrast matrix. The posterior predictions matrix
#'   is post multiplied by the contrast matrix, so they must be conformable.
#'   The posterior predictions matrix has a separate column for each row in the
#'   \code{at} or \code{add} object, so the contrast matrix should have the same
#'   number of rows. It can have multiple columns, if you desire multiple specific
#'   contrasts.
#' @param ROPE Either left as \code{NULL}, the default, or a numeric vector of
#'   length 2, specifying the lower and upper thresholds for the
#'   Region of Practical Equivalence (ROPE).
#' @param MID Either left as \code{NULL}, the default, or a numeric vector of
#'   length 2, specifying the lower and upper thresholds for a
#'   Minimally Important Difference (MID). Unlike the ROPE, percentages for
#'   the MID are calculated as at or exceeding the bounds specified by this
#'   argument, whereas the ROPE is the percentage of the posterior at or inside
#'   the bounds specified.
#' @param subset A character string that is a valid \code{R} expression
#'   used to subset the dataset passed in \code{newdata},
#'   prior to analysis. Defaults to \code{NULL}.
#' @param seed Argument that controls whether (and if so what) random seed
#'   to use. This does not matter when using fixed effects only. However,
#'   when using Monte Carlo integration to integrate out random effects from
#'   mixed effects models, it is critical if you are looking at a continuous
#'   marginal effect with some small offset value as otherwise the
#'   Monte Carlo error from one set of predictions to another may exceed
#'   the true predicted difference.
#'   If \code{seed} is left missing, the default, than a single, random integer
#'   between +\- 1e7 is chosen and used to set the seed before each
#'   prediction. If manually chosen (recommended for reproducibility),
#'   the seed should either be a single value, in which case this single
#'   value is used to set the seed before each prediction.
#'   Alternately, it can be a vector of seeds with either the same length
#'   as the number of rows in \code{at} or \code{add}, whichever was specified.
#'   This is probably generally not what you want, as it means that even for
#'   the same input data, you would get slightly different predictions
#'   (when integrating out random effects) due to Monte Carlo variation.
#'   Finally, rather than being missing, you can explicitly set
#'   \code{seed = NULL}, if you do not want any seed to be set.
#'   This would be fine, for instance, when only using fixed effects,
#'   or if you know what you are doing and intend that behavior when
#'   integrating out random effects.
#' @param ... Additional arguments passed on to \code{\link{.predict}}.
#' @importFrom stats model.frame
#' @importFrom data.table as.data.table copy :=
#' @return A list. TODO describe more.
#' @export
#' @examples
#' \dontrun{
#' #### Testing ####
#' ## sample data and logistic model with brms
#' set.seed(1234)
#' Tx <- rep(0:1, each = 50)
#' ybin <- c(rep(0:1, c(40,10)), rep(0:1, c(10,40)))
#' logitd <- data.frame(Tx = Tx, ybin = ybin)
#' logitd$x <- rnorm(100, mean = logitd$ybin, sd = 2)
#'
#' mbin <- brms::brm(ybin ~ Tx + x, data = logitd, family = brms::bernoulli())
#'
#' summary(mbin)
#'
#' ## predictions + summary
#' test1 <- brmsmargins:::.predict(mbin,
#'          model.frame(mbin),
#'          summarize = TRUE, posterior = FALSE, dpar = NULL,
#'          resample = 0L)
#' test1
#'
#' ## check that bootstrapping the sample / population assumed as part of the
#' ## AME indeed increases the uncertainty interval
#' ## TODO: point estimates (M, Mdn) should be based on the actual data
#' ## not the bootstrapped
#' test2 <- brmsmargins:::.predict(mbin,
#'          model.frame(mbin),
#'          summarize = TRUE, posterior = FALSE, dpar = NULL,
#'          resample = 100L, seed = 1234)
#' test2
#'
#'
#' ## now check AME for Tx
#' tmp <- brmsmargins(
#'   object = mbin,
#'   at = data.table::data.table(Tx = 0:1),
#'   contrasts = matrix(c(-1, 1), nrow = 2),
#'   ROPE = c(-.05, +.05),
#'   MID = c(-.10, +.10))
#'
#' tmp$Summary
#' tmp$ContrastSummary ## Tx AME
#'
#'
#' ## now check AME for Tx with bootstrapping the AME population
#' tmpalt <- brmsmargins(
#'   object = mbin,
#'   at = data.table::data.table(Tx = 0:1),
#'   contrasts = matrix(c(-1, 1), nrow = 2),
#'   ROPE = c(-.05, +.05),
#'   MID = c(-.10, +.10),
#'   resample = 100L)
#'
#' tmpalt$Summary
#' tmpalt$ContrastSummary ## Tx AME
#'
#' ## now check AME for continuous predictor, x
#' ## use .01 as an approximation for first derivative
#' ## 1 / .01 in the contrast matrix to get back to a one unit change metric
#' tmp2 <- brmsmargins(
#'   object = mbin,
#'   add = data.table::data.table(x = c(0, .01)),
#'   contrasts = matrix(c(-1/.01, 1/.01), nrow = 2),
#'   ROPE = c(-.05, +.05),
#'   MID = c(-.10, +.10))
#'
#' tmp2$ContrastSummary ## x AME
#'
#' if (FALSE) {
#'   library(lme4)
#'   data(sleepstudy)
#'   fit <- brms::brm(Reaction ~ 1 + Days + (1+ Days | Subject),
#'              data = sleepstudy,
#'              cores = 4)
#'
#'   summary(fit, prob = 0.99)
#'
#'   tmp <- brmsmargins(
#'     object = fit,
#'     at = data.table::data.table(Days = 0:1),
#'     contrasts = matrix(c(-1, 1), nrow = 2),
#'     ROPE = c(-.05, +.05),
#'     MID = c(-.10, +.10), CIType = "ETI", effects = "integrateoutRE", k = 5L)
#'
#'   tmp$Summary
#'   tmp$ContrastSummary
#'   }
#' }
brmsmargins <- function(object, at = NULL, add = NULL, newdata = model.frame(object),
                        CI = .99, CIType = "HDI", contrasts = NULL,
                        ROPE = NULL, MID = NULL, subset = NULL, seed, ...) {
  .assertbrmsfit(object)
  chknewdata <- .checktab(newdata)
  if (isTRUE(nzchar(chknewdata))) {
    stop(paste0("newdata: ", chknewdata))
  }
  newdata <- copy(as.data.table(newdata))

  if (isFALSE(missingArg(seed))) {
    if (isFALSE(is.null(seed))) {
      stopifnot(
        identical(length(seed), 1L) ||
          identical(length(seed), nrow(at)) ||
          identical(length(seed), nrow(add)))
    }
  } else if (isTRUE(missingArg(seed))) {
    ## create a random seed somewhere between +/- 1e7
    seed <- ceiling(runif(1, -1e7, 1e7))
  }

  if (isFALSE(is.null(subset))) {
    if (isFALSE(is.character(subset))) {
      stop("subset must be a character string that results in a logical statement evaluated in the data.")
    }
    newdata <- subset(
      newdata,
      subset = eval(parse(text = subset)))
  }

  if (isFALSE(is.null(at))) {
    chkat <- .checktab(at)
    if (isTRUE(nzchar(chkat))) {
      stop(paste0("at: ", chkat))
    }
    at <- copy(as.data.table(at))
  }

  if (isFALSE(is.null(add))) {
    chkadd <- .checktab(add)
    if (isTRUE(nzchar(chkadd))) {
      stop(paste0("add: ", chkadd))
    }
    add <- copy(as.data.table(add))
  }

  if (isFALSE(is.null(contrasts))) {
    chkcontrasts <- .checktab(contrasts, requireNames = FALSE)
    if (isTRUE(nzchar(chkcontrasts))) {
      stop(paste0("contrasts: ", chkcontrasts))
    }
    contrasts <- as.matrix(contrasts)
    if (isTRUE(is.null(colnames(contrasts)))) {
      colnames(contrasts) <- paste0("Contrast_", seq_len(ncol(contrasts)))
    }
  }

  if (isFALSE(is.null(at)) && isFALSE(is.null(add))) {
    stop(paste("Currently only 'at' or 'add' may be specified.",
               "Including both is not currently supported.",
               sep = "\n"))
  }

  if (isFALSE(is.null(at))) {
    out <- vector("list", nrow(at))
    for (i in seq_len(nrow(at))) {
      for (v in names(at)) {
        newdata[, (v) := at[i, get(v)]]
      }
      if (isFALSE(is.null(seed))) {
        if (isTRUE(length(seed) > 1)) {
          set.seed(seed[i])
        } else {
          set.seed(seed)
        }
      }
      out[[i]] <- .predict(
        object, data = newdata,
        ROPE = ROPE, MID = MID, posterior = TRUE,
        CI = CI, CIType = CIType, ...)
    }

    post <- do.call(cbind, lapply(out, `[[`, "Posterior"))
    s <- do.call(rbind, lapply(out, `[[`, "Summary"))

    rm(out)
    gc()
  }

  if (isFALSE(is.null(add))) {
    out <- vector("list", nrow(add))
    for (i in seq_len(nrow(add))) {
      tmp <- copy(newdata)
      for (v in names(add)) {
        value <- add[i, get(v)]
        tmp[, (v) := get(v) + value]
      }
      if (isFALSE(is.null(seed))) {
        if (isTRUE(length(seed) > 1)) {
          set.seed(seed[i])
        } else {
          set.seed(seed)
        }
      }
      out[[i]] <- .predict(
        object, data = tmp,
        ROPE = ROPE, MID = MID, posterior = TRUE,
        CI = CI, CIType = CIType, ...)
    }

    post <- do.call(cbind, lapply(out, `[[`, "Posterior"))
    s <- do.call(rbind, lapply(out, `[[`, "Summary"))

    rm(out)
    gc()
  }

  if (isFALSE(is.null(contrasts))) {
    res <- post %*% contrasts
    contrastsum <- apply(res, 2, bsummary,
          CI = CI, CIType = CIType,
          ROPE = ROPE, MID = MID)
    contrastsum <- do.call(rbind, contrastsum)
    contrastsum[, Label := colnames(contrasts)]
  } else {
    res <- NA
    contrastsum <- NA
  }

  out <- list(
    Posterior = post,
    Summary = s,
    Contrasts = res,
    ContrastSummary = contrastsum)

  return(out)
}
