library(brms)
library(data.table)
library(bayestestR)
library(emmeans)
library(extraoperators)
library(Rcpp)

## testing c++ row wise bootstrap and average function
## to be used on a matrix of posterior predictons where
## Rows = posterior samples and
## Columns = different people / observations for which predictions were generated
x <- matrix(1:9, nrow = 3, byrow=TRUE)
replicate(10, rbootmeans(x))

cppFunction("
NumericVector rbootmeans(NumericMatrix x) {
  NumericVector out( x.nrow() );
  int n = x.nrow();
  for(int i = 0; i < n; i++){
    NumericVector tmp = x(i, _);
    out[i] = mean(sample(tmp, tmp.size(), true));
  }
  return out;
}")

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

.predict <- function(object, data, summarize = TRUE, posterior = FALSE, dpar = NULL, re_formula = NULL, resample = 0L, seed, ...) {
  out <- list(
    Summary = NULL,
    Posterior = NULL)

  out$Posterior <- fitted(
    object = object,
    newdata = data,
    re_formula = re_formula,
    scale = "response",
    dpar = dpar,
    summary = FALSE)

  if (isTRUE(resample == 0)) {
    out$Posterior <- rowMeans(out$Posterior, na.rm = TRUE)
  } else if (isTRUE(resample > 0)) {
    if (isFALSE(missingArg(seed))) {
      set.seed(seed)
    }

    yhat <- matrix(NA_real_, nrow = nrow(out$Posterior), ncol = resample)
    for (i in 1:resample) {
      yhat[, i] <- rbootmeans(out$Posterior)
    }
    
    out$Posterior <- as.vector(yhat)
    rm(yhat)
  }

  if (isTRUE(summarize)) {
    out$Summary <- bsummary(out$Posterior, ...)
  } 
  if (isFALSE(posterior)) {
    out$Posterior <- NULL
  }
  return(out)  
}

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

#' brmsmargins
#'
#' @param m model
#' @param cond stuff
#' @param ci ci
#' @param CIType the type
#' @param prob is it a probability
#' @param rope rope
#' @param mid mid
#' @export

## object <- mbin
## at <- data.table(Tx = 0:1)
## add <- NULL
## newdata <- model.frame(object)
## contrasts <- matrix(c(-1, 1), nrow = 2)

## object <- mbin
## at <- expand.grid(Tx = 0:1, x = c(-4, +4))
## add <- NULL
## newdata <- model.frame(object)
## contrasts <- matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), nrow = 4)
brmsmargins <- function(object, at = NULL, add = NULL, newdata = model.frame(object),
                        CI = .99, CIType = "HDI", contrasts = NULL,
                        ROPE = NULL, MID = NULL, subset = NULL, ...) {
  
  chknewdata <- .checktab(newdata)
  if (isTRUE(nzchar(chknewdata))) {
    stop(paste0("newdata: ", chknewdata))
  }
  newdata <- copy(as.data.table(newdata))

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
  }

  if (isFALSE(is.null(at)) && isFALSE(is.null(add))) {
    stop(paste("Currently only 'at' or 'add' may be specified.",
               "Including both is not currently supported.",
               sep = "\n"))
  }

  if (isFALSE(is.null(at))) {
    out <- vector("list", nrow(at))
    for (i in 1:nrow(at)) {
      for (v in names(at)) {
        newdata[, (v) := at[i, get(v)]]
      }
      out[[i]] <- .predict(object, data = newdata,
                           ROPE = ROPE, MID = MID,
                           posterior = TRUE, ...)
    }
    
    post <- do.call(cbind, lapply(out, `[[`, "Posterior"))
    s <- do.call(rbind, lapply(out, `[[`, "Summary"))

    rm(out)
    gc()
  }
  
  if (isFALSE(is.null(add))) {
    out <- vector("list", nrow(add))
    for (i in 1:nrow(add)) {
      tmp <- copy(newdata)
      for (v in names(add)) {
        value <- add[i, get(v)]
        tmp[, (v) := get(v) + value]
      }
      out[[i]] <- .predict(object, data = tmp,
                           ROPE = ROPE, MID = MID,
                           posterior = TRUE, ...)
    }

    post <- do.call(cbind, lapply(out, `[[`, "Posterior"))
    s <- do.call(rbind, lapply(out, `[[`, "Summary"))
    
    rm(out)
    gc()    
  }

  if (isFALSE(is.null(contrasts))) {
    res <- post %*% contrasts
    contrastsum <- apply(res, 2, bsummary,
          CI = CI, type = CIType,
          ROPE = ROPE, MID = MID)
    
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

#### Testing ####

## sample data and logistic model with brms
set.seed(1234)
logitd <- data.frame(
  Tx = rep(0:1, each = 50),
  ybin = c(rep(0:1, c(40,10)),
           rep(0:1, c(10,40))))
logitd$x <- rnorm(100, mean = logitd$ybin, sd = 2)

mbin <- brm(ybin ~ Tx + x, data = logitd, family = bernoulli())

summary(mbin)

## predictions + summary 
test1 <- .predict(mbin,
         model.frame(mbin),
         summarize = TRUE, posterior = FALSE, dpar = NULL, re_formula = NULL,
         resample = 0L)

test1

## check that bootstrapping the sample / population assumed as part of the
## AME indeed increases the uncertainty interval
## TODO: point estimates (M, Mdn) should be based on the actual data
## not the bootstrapped
test2 <- .predict(mbin,
         model.frame(mbin),
         summarize = TRUE, posterior = FALSE, dpar = NULL, re_formula = NULL,
         resample = 100L, seed = 1234)

test2


## now check AME for Tx
tmp <- brmsmargins(
  object = mbin,
  at = data.table(Tx = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp$Summary
tmp$ContrastSummary ## Tx AME


## now check AME for Tx
tmpalt <- brmsmargins(
  object = mbin,
  at = data.table(Tx = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10),
  resample = 100L)

tmpalt$Summary
tmpalt$ContrastSummary ## Tx AME

## now check AME for continuous predictor, x
## use .01 as an approximation for first derivative
## 1 / .01 in the contrast matrix to get back to a one unit change metric
tmp2 <- brmsmargins(
  object = mbin,
  add = data.table(x = c(0, .01)),
  contrasts = matrix(c(-1/.01, 1/.01), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp2$ContrastSummary ## x AME


mbin2 <- brm(Sepal.Length ~ Species, data = iris, family = bernoulli())

summary(mbin)

tmp <- brmsmargins(
  object = mbin,
  at = data.table(vs = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp$Summary
tmp$ContrastSummary



library(lme4)
data(sleepstudy)
fit <- brm(Reaction ~ 1 + Days + (1+ Days | Subject), 
           data = sleepstudy,
           cores = 4)

summary(fit) 

tmp <- brmsmargins(
  object = fit,
  at = data.table(Days = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp$Summary
tmp$ContrastSummary

