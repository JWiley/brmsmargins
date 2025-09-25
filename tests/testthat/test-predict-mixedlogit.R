skip_on_cran()

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  backend <- "rstan"
  ## if using rstan backend, models can crash on Windows
  ## so skip if on windows and cannot use cmdstanr
  skip_on_os("windows")
} else {
  if (isFALSE(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))) {
    backend <- "cmdstanr"
  }
}

dlogit <- withr::with_seed(
  seed = 12345, code = {
    nGroups <- 100
    nObs <- 20
    theta.location <- matrix(rnorm(nGroups * 2), nrow = nGroups, ncol = 2)
    theta.location[, 1] <- theta.location[, 1] - mean(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] - mean(theta.location[, 2])
    theta.location[, 1] <- theta.location[, 1] / sd(theta.location[, 1])
    theta.location[, 2] <- theta.location[, 2] / sd(theta.location[, 2])
    theta.location <- theta.location %*% chol(matrix(c(1.5, -.25, -.25, .5^2), 2))
    theta.location[, 1] <- theta.location[, 1] - 2.5
    theta.location[, 2] <- theta.location[, 2] + 1
    d <- data.table(
      x = rep(rep(0:1, each = nObs / 2), times = nGroups))
    d[, ID := rep(seq_len(nGroups), each = nObs)]

    for (i in seq_len(nGroups)) {
      d[ID == i, y := rbinom(
        n = nObs,
        size = 1,
        prob = plogis(theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

res.samp <- dlogit[, .(M = mean(y)), by = .(ID, x)][, .(M = mean(M)), by = x]

suppressWarnings(
  mlogit <- brms::brm(
    y ~ 1 + x + (1 + x | ID), family = "bernoulli",
    data = dlogit, iter = 1000, warmup = 500, seed = 1234,
    chains = 2, backend = backend, save_pars = save_pars(all = TRUE),
    silent = 2, refresh = 0)
)

preddat <- data.frame(y = c(1, 1), x = c(0, 1), ID = 999)

res.integrate <- withr::with_seed(
  seed = 1234, {
    test0 <- prediction(object = mlogit, data = preddat[1, ], posterior = TRUE,
                      effects = "integrateoutRE", k = 100L, CI = 0.95, CIType = "ETI")
    test1 <- prediction(object = mlogit, data = preddat[2, ], posterior = TRUE,
                      effects = "integrateoutRE", k = 100L, CI = 0.95, CIType = "ETI")
    ame <- list(Summary = NULL, Posterior = test1$Posterior - test0$Posterior)
    ame$Summary <- bsummary(ame$Posterior, CI = 0.95, CIType = "ETI")

    list(
      Summary = rbind(
        test0$Summary, test1$Summary, ame$Summary),
      Posterior = cbind(
        test0$Posterior, test1$Posterior, ame$Posterior))
  })

res.fixedonly <- withr::with_seed(
  seed = 1234, {
    test0 <- prediction(object = mlogit, data = preddat[1, ], posterior = TRUE,
                      effects = "fixedonly", CI = 0.95, CIType = "ETI")
    test1 <- prediction(object = mlogit, data = preddat[2, ], posterior = TRUE,
                      effects = "fixedonly", CI = 0.95, CIType = "ETI")
    ame <- list(Summary = NULL, Posterior = test1$Posterior - test0$Posterior)
    ame$Summary <- bsummary(ame$Posterior, CI = 0.95, CIType = "ETI")

    list(
      Summary = rbind(
        test0$Summary, test1$Summary, ame$Summary),
      Posterior = cbind(
        test0$Posterior, test1$Posterior, ame$Posterior))
  })

test_that("prediction works to integrate out random effects in multilevel logistic models", {
  expect_type(res.integrate, "list")
  expect_equal(
    c(ndraws(mlogit), 3L),
    dim(res.integrate$Posterior))
  expect_true(all(
    res.integrate$Posterior[, 1:2] >= 0 &
      res.integrate$Posterior[, 1:2] <= 1))
  expect_true(all(
    res.integrate$Summary$M >= 0 &
      res.integrate$Summary$M <= 1))

  expect_true(abs(res.integrate$Summary$M[1] - res.samp$M[1]) < .01)
  expect_true(abs(res.integrate$Summary$M[2] - res.samp$M[2]) < .01)
  expect_true(abs(res.integrate$Summary$M[3] -
                    (res.samp$M[2] - res.samp$M[1])) < .01)
})

test_that("prediction works with fixed effects only in multilevel logistic models", {
  expect_type(res.fixedonly, "list")
  expect_equal(
    c(ndraws(mlogit), 3L),
    dim(res.fixedonly$Posterior))
  expect_true(all(
    res.fixedonly$Posterior[, 1:2] >= 0 &
      res.fixedonly$Posterior[, 1:2] <= 1))
  expect_true(all(
    res.fixedonly$Summary$M >= 0 &
      res.fixedonly$Summary$M <= 1))
  expect_true(res.fixedonly$Summary$M[1] < res.integrate$Summary$M[1])
  expect_true(res.fixedonly$Summary$M[2] < res.integrate$Summary$M[2])
})

h <- .001
ames <- brmsmargins(
  object = mlogit,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME time" = c(-1 / h, 1 / h)),
  effects = "integrateoutRE",
  k = 100L,
  seed = 1234
)

test_that("brmsmargins works with random slope logit models", {
  expect_type(ames, "list")
  expect_equal(
    ndraws(mlogit),
    nrow(ames$Posterior))
  expect_true(all(
    ames$Posterior[, 1:2] >= 0 &
      ames$Posterior[, 1:2] <= 1))
  expect_true(all(
    ames$ContrastSummary$M >= 0 &
      ames$ContrastSummary$M <= 1))
  expect_true(abs(ames$ContrastSummary$M - 0.11) < .02)
})

suppressWarnings(
  mlogit.intonly <- brms::brm(
  y ~ 1 + x + (1 | ID), family = "bernoulli",
  data = dlogit, seed = 1234,
  chains = 2, backend = backend, save_pars = save_pars(all = TRUE),
  silent = 2, refresh = 0)
)

h <- .001
ames <- brmsmargins(
  object = mlogit.intonly,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME time" = c(-1 / h, 1 / h)),
  effects = "integrateoutRE",
  k = 100L,
  seed = 1234
)

test_that("brmsmargins works with intercept only logit models", {
  expect_type(ames, "list")
  expect_equal(
    ndraws(mlogit.intonly),
    nrow(ames$Posterior))
  expect_true(all(
    ames$Posterior[, 1:2] >= 0 &
      ames$Posterior[, 1:2] <= 1))
  expect_true(all(
    ames$ContrastSummary$M >= 0 &
      ames$ContrastSummary$M <= 1))
  expect_true(abs(ames$ContrastSummary$M - 0.11) < .02)
})
