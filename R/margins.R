## make Rcmd check happy
utils::globalVariables(c("Label", "variable"))

#' Calculate Marginal Effects from 'brms' Models
#'
#' This function is designed to help calculate marginal effects
#' including average marginal effects (AMEs) from \code{brms} models.
#' Arguments are labeled as \emph{required} when it is required that the
#' user directly specify the argument. Arguments are labeled as
#' \emph{optional} when either the argument is optional or there are
#' sensible default values so that users do not typically need to specify
#' the argument.
#'
#' The main parts required for the function are a fitted model object,
#' (via the \code{object} argument) a dataset to be used for prediction,
#' (via the \code{newdata} argument which defaults to the model frame),
#' and a dataset passed to either \code{at} or \code{add}.
#' The steps are as follows:
#' \enumerate{
#'   \item Check that the function inputs (model object, data, etc.) are valid.
#'   \item Take the dataset from the \code{newdata} argument and either
#'     add the values from the first row of \code{add} or replace the values
#'     using the first row of \code{at}. Only variables specified in
#'     \code{at} or \code{add} are modified. Other variables are left as is.
#'   \item Use the \code{fitted()} function to generate predictions based on
#'     this modified dataset. If \code{effects} is set to \dQuote{fixedonly}
#'     (meaning only generate predictions using fixed effects)
#'     or to \dQuote{includeRE}
#'     (meaning generate predictions using fixed and random effects),
#'     then predictions are generated entirely using the \code{fitted()}
#'     function and are, typically back transformed to the response scale.
#'     For mixed effects models with fixed and random effects where
#'     \code{effects} is set to \dQuote{integrateoutRE}, then \code{fitted()}
#'     is only used to generate predictions using the fixed effects on the linear
#'     scale. For each prediction generated, the random effects are integrated out
#'     by drawing \code{k} random samples from the model assumed random effect(s)
#'     distribution. These are added to the fixed effects predictions,
#'     back transformed, and then averaged over all \code{k} random samples to
#'     perform numerical Monte Carlo integration.
#'   \item All the predictions for each posterior draw, after any back transformation
#'     has been applied, are averaged, resulting in one, marginal value for each
#'     posterior draw. These are marginal predictions. They are average marginal
#'     predictions if averaging over the sample dataset, or may be marginal predictions
#'     at the means, if the initial input dataset used mean values, etc.
#'   \item Steps two to four are repeated for each row of \code{at} or \code{add}.
#'     Results are combined into a matrix where the columns are different
#'     rows from \code{at} or \code{add} and the rows are different posterior
#'     draws.
#'   \item If contrasts were specified, using a contrast matrix, the
#'     marginal prediction matrix is post multiplied by the contrast matrix.
#'     Depending on the choice(s) of \code{add} or \code{at} and the
#'     values in the contrast matrix, these can then be
#'     average marginal effects (AMEs) by using numerical integration
#'     (\code{add} with 0 and a very close to 0 value) or
#'     discrete difference (\code{at} with say 0 and 1 as values)
#'     for a given predictor(s).
#'   \item The marginal predictions and the contrasts, if specified are
#'     summarized.
#' }
#'
#' Although \code{brmsmargins()} is focused on helping to calculate
#' marginal effects, it can also be used to generate marginal predictions,
#' and indeed these marginal predictions are the foundation of any
#' marginal effect estimates. Through manipulating the input data,
#' \code{at} or \code{add} and the contrast matrix, other types of estimates
#' averaged or weighting results in specific ways are also possible.
#'
#' @param object A \emph{required} argument specifying a fitted \code{brms} model object.
#' @param at An \emph{optional} argument (but note, either \code{at} or \code{add} are
#'   \emph{required}) specifying an object inheriting from data frame indicating
#'   the values to hold specific variables at when calculating average
#'   predictions. This is intended for AMEs from categorical variables.
#' @param wat An \emph{optional} list with named elements including one element named,
#'   \dQuote{ID} with a single character string, the name of the variable
#'   in the model frame that is the ID variable. Additionally,
#'   there should be one or more named elements, named after variables
#'   in the model (and specified in the \code{at} argument), that
#'   contain a \code{data.table} or \code{data.frame} with three
#'   variables: (1) the ID variable giving IDs, (2) the values
#'   specified for the variable in the \code{at} argument, and
#'   (3) the actual values to be substituted for each ID.
#'   \code{wat} cannot be non null unless \code{at} also is non null.
#' @param add An \emph{optional} argument (but note, either \code{at} or \code{add} are
#'   \emph{required}) specifying an object inheriting from data frame indicating
#'   the values to add to specific variables at when calculating average
#'   predictions. This is intended for AMEs for continuous variables.
#' @param newdata An \emph{optional} argument specifying an object inheriting
#'   from data frame indicating the baseline values to use for predictions and AMEs.
#'   It uses a sensible default: the model frame from the \code{brms}
#'   model object passed on the \code{object} argument.
#' @param CI An \emph{optional} argument with a numeric value specifying the width
#'   of the credible interval. Defaults to \code{0.99}. This default is arbitrary,
#'   but is purposefully higher than the common \code{0.95} to encourage science
#'   with greater acknowledgment of uncertainty or larger sample sizes (ideally).
#' @param CIType An \emph{optional} argument, a character string specifying the
#'   type of credible interval (e.g., highest density interval). It is passed down to
#'   \code{\link{bsummary}} which in turn passes it to
#'   \code{\link[bayestestR]{ci}}. Defaults to \dQuote{HDI}.
#' @param contrasts An \emph{optional} argument specifying a contrast matrix.
#'   The posterior predictions matrix
#'   is post multiplied by the contrast matrix, so they must be conformable.
#'   The posterior predictions matrix has a separate column for each row in the
#'   \code{at} or \code{add} object, so the contrast matrix should have the same
#'   number of rows. It can have multiple columns, if you desire multiple specific
#'   contrasts.
#' @param ROPE An \emph{optional} argument, that can either be left as \code{NULL},
#'   the default, or a numeric vector of length 2, specifying the
#'   lower and upper thresholds for the
#'   Region of Practical Equivalence (ROPE).
#' @param MID An \emph{optional} argument, that can either left as \code{NULL},
#'   the default, or a numeric vector of length 2, specifying the
#'   lower and upper thresholds for a
#'   Minimally Important Difference (MID). Unlike the ROPE, percentages for
#'   the MID are calculated as at or exceeding the bounds specified by this
#'   argument, whereas the ROPE is the percentage of the posterior at or inside
#'   the bounds specified.
#' @param subset An \emph{optional} argument, a character string that is a
#'   valid \code{R} expression used to subset the dataset passed in \code{newdata},
#'   prior to analysis. Defaults to \code{NULL}.
#' @param dpar An \emph{optional} argument giving the parameter passed on to the \code{dpar}
#'   argument of \code{fitted()} in brms. Defaults to \code{NULL},
#'   indicating the mean or location parameter typically.
#' @param seed An \emph{optional} argument that controls whether (and if so what) random seed
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
#' @param verbose An \emph{optional} argument, a logical value whether to print
#'   more verbose messages. Defaults to \code{FALSE} which is quieter. Set to
#'   \code{TRUE} for more messages to be printed where relevant.
#' @param ... An \emph{optional} argument, additional arguments passed on to
#'   \code{\link{prediction}}. In particular, the \code{effects} argument of [prediction()]
#'   is important for mixed effects models to control how random effects
#'   are treated in the predictions, which subsequently changes the
#'   marginal effect estimates.
#' @importFrom stats model.frame runif
#' @importFrom data.table as.data.table is.data.table copy :=
#' @importFrom extraoperators %gele% %nin%
#' @return A list with four elements.
#' \itemize{
#'   \item{\code{Posterior}}{Posterior distribution of all predictions. These predictions default to fixed effects only, but by specifying options to [prediction()] they can include random effects or be predictions integrating out random effects.}
#'   \item{\code{Summary}}{A summary of the predictions.}
#'   \item{\code{Contrasts}}{Posterior distribution of all contrasts, if a contrast matrix was specified.}
#'   \item{\code{ContrastSummary}}{A summary of the posterior distribution of all contrasts, if specified}
#' }
#' @export
#' @references
#' Pavlou, M., Ambler, G., Seaman, S., & Omar, R. Z. (2015)
#' \doi{10.1186/s12874-015-0046-6}
#' \dQuote{A note on obtaining correct marginal predictions from a random intercepts model for binary outcomes}
#' and
#' Skrondal, A., & Rabe-Hesketh, S. (2009)
#' \doi{10.1111/j.1467-985X.2009.00587.x}
#' \dQuote{Prediction in multilevel generalized linear models}
#' and
#' Norton EC, Dowd BE, Maciejewski ML. (2019)
#' \doi{10.1001/jama.2019.1954}
#' \dQuote{Marginal Effects—Quantifying the Effect of Changes in Risk Factors in Logistic Regression Models}
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
#'   fit <- brms::brm(Reaction ~ 1 + Days + (1 + Days | Subject),
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
brmsmargins <- function(object, at = NULL, wat = NULL, add = NULL, newdata = model.frame(object),
                        CI = .99, CIType = "HDI", contrasts = NULL,
                        ROPE = NULL, MID = NULL,
                        subset = NULL, dpar = NULL, seed, verbose = FALSE,
                       ...) {
  if (isTRUE(missing(object))) {
    stop(paste(
      "'object' is a required argument and cannot be missing;",
      "  it should be a saved model fit from brms. For example:",
      "  m <- brm(y ~ x, data = yourdata)",
      "  See ?brmsmargins or the website articles (vignettes) for details.",
      "  https://joshuawiley.com/brmsmargins/", sep = "\n"))
  }
  .assertbrmsfit(object)

  chknewdata <- .checktab(newdata)
  if (isTRUE(nzchar(chknewdata))) {
    stop(paste0("newdata: ", chknewdata))
  }
  newdata <- copy(as.data.table(newdata))

  if (isFALSE(missing(seed))) {
    if (isFALSE(is.null(seed))) {
      stopifnot(
        identical(length(seed), 1L) ||
          identical(length(seed), nrow(at)) ||
          identical(length(seed), nrow(add)))
    }
  } else if (isTRUE(missing(seed))) {
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

  # error if missing both at and add
  if (isTRUE(is.null(at)) && isTRUE(is.null(add))) {
    stop(paste("You must specify either 'at' or 'add'",
               "See ?brmsmargins or vignettes for help.",
               sep = "\n"))
  }

  if (isFALSE(is.null(add)) && isTRUE(is.null(contrasts))) {
    if (isTRUE(verbose)) {
      message(paste(
        "It is unusual to specify 'add' without 'contrasts'.",
        "Without 'contrasts', only marginal predictions will be generated.",
        "If predictions are desired, consider using prediction() directly.",
        "To suppress this message, set 'verbose = FALSE'", sep = "\n"))
    }
  }

  if (isFALSE(CI %gele% c(0, 1))) {
    stop(paste(
      sprintf("'CI' is %s", as.character(CI)),
      "'CI' should specify the desired credible interval as a numeric value in (0, 1)",
      "See ?bayestestR::ci for details",
      sep = "\n"))
  }

  if (isFALSE(CIType %in% c("HDI", "ETI", "BCI", "SI"))) {
    stop(paste(
      sprintf("'CIType' is %s", as.character(CIType)),
      "'CIType' should be one of 'HDI' (default), 'ETI', 'BCI', or 'SI'",
      "See ?bayestestR::ci for details",
      sep = "\n"))
  }

  if (isFALSE(is.null(wat))) {
    if (isTRUE(is.null(at))) {
      stop("If 'wat' is specified, 'at' also must be specified.")
    }

    test.wat.type <- isTRUE(is.list(wat))
    test.wat.id <- isTRUE("ID" %in% names(wat))

    if (isFALSE(test.wat.type) || isFALSE(test.wat.id)) {
      stop(paste(
        "'wat' should be a list with named elements",
        "including 'ID' giving the name of the ID variable and",
        "separate elements (each a data.frame or data.table) containing the IDs,",
        "the values specified in 'at' and the true values to use by ID.",
        sep = "\n"))
    }
  }

  if (isFALSE(is.null(at))) {
    out <- vector("list", nrow(at))
    for (i in seq_len(nrow(at))) {
      for (v in names(at)) {
        fill_in_value <- at[i, get(v)]
        if (isTRUE(is.null(wat) || v %nin% names(wat))) {
          newdata[, (v) := fill_in_value]
        } else {
          if (isFALSE(is.data.table(wat[[v]]))) {
            wat[[v]] <- as.data.table(wat[[v]])
          }
          for (useid in unique(newdata[[wat$ID]])) {
            newdata[get(wat$ID) == useid,
            (v) := wat[[v]][variable == fill_in_value & get(wat$ID) == useid, value]]
          }
        }
      }
      if (isFALSE(is.null(seed))) {
        if (isTRUE(length(seed) > 1)) {
          set.seed(seed[i])
        } else {
          set.seed(seed)
        }
      }
      out[[i]] <- prediction(
        object, data = newdata,
        ROPE = ROPE, MID = MID, posterior = TRUE,
        CI = CI, CIType = CIType, dpar = dpar, ...)
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
      out[[i]] <- prediction(
        object, data = tmp,
        ROPE = ROPE, MID = MID, posterior = TRUE,
        CI = CI, CIType = CIType, dpar = dpar, ...)
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
