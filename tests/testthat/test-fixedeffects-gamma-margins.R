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

dgamma <- withr::with_seed(
  seed = 12345, code = {
    nGroups <- 2L
    nObs <- 200L
    d <- data.table(x = rep(0:1, each = nObs))
    d[, y := rgamma(n = nObs * nGroups, shape = 2 + x, rate = 5)]
    copy(d)
  })

suppressWarnings(
  bayes.gamma <- brms::brm(
    formula = y ~ x, family = "Gamma",
    data = dgamma, iter = 1000, warmup = 500, seed = 1234,
    chains = 2, backend = backend, save_pars = save_pars(all = TRUE),
    silent = 2, refresh = 0)
)

h <- .001
margins.bayes <- brmsmargins(bayes.gamma,
            add = data.table(x = c(0, h)),
            CI = 0.95,
            contrasts = cbind(AME = c(-1 / h, 1 / h)))
ame.bayes <- margins.bayes$ContrastSummary

test_that("brmsmargins runs for a fixed effects gamma model", {
  expect_type(margins.bayes, "list")
  expect_equal(nrow(margins.bayes$Posterior),
               ndraws(bayes.gamma))

  expect_equal(nrow(margins.bayes$Contrasts),
               ndraws(bayes.gamma))

  expect_true(all(margins.bayes$M >= 0 & margins.bayes$M <= 1))
  expect_true(all(margins.bayes$Mdn >= 0 & margins.bayes$Mdn <= 1))
  expect_true(all(margins.bayes$LL >= 0 & margins.bayes$LL <= 1))
  expect_true(all(margins.bayes$UL >= 0 & margins.bayes$UL <= 1))

  expect_true(ame.bayes$M >= 0 && ame.bayes$M <= 1)
  expect_true(ame.bayes$Mdn >= 0 && ame.bayes$Mdn <= 1)
  expect_true(ame.bayes$LL >= 0 && ame.bayes$LL <= 1)
  expect_true(ame.bayes$UL >= 0 && ame.bayes$UL <= 1)
})

test_that("brmsmargins CI include true difference for a gamma regression model", {

  expect_true(ame.bayes$LL <= 1 / 5)

  expect_true(ame.bayes$UL >= 1 / 5)
})


skip_if_not_installed("margins")

m.freq <- stats::glm(y ~ x, data = dgamma, family = Gamma())
ame.freq <- summary(margins::margins(m.freq))

test_that("brmsmargins roughly matches margins on a frequentist fixed effects gamma regression model", {

  expect_true(abs(ame.freq$AME - ame.bayes$M) < .05)

  expect_true(abs(ame.freq$lower - ame.bayes$LL) < .05)

  expect_true(abs(ame.freq$upper - ame.bayes$UL) < .05)
})
