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

dpois <- withr::with_seed(
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
      d[ID == i, y := rpois(
        n = nObs,
        lambda = exp(1 + theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

res.samp <- dpois[, .(M = mean(y)), by = .(ID, x)][, .(M = mean(M)), by = x]
res.samp <- res.samp[, .(
  Label = c("Intercept", "x"),
  Est = c(log(M[x == 0]),
          log( (M[x == 1] ) / (M[x == 0]) )))]

suppressWarnings(
  mpois <- brms::brm(
    y ~ 1 + x + (1 + x | ID), family = "poisson",
    data = dpois, iter = 1000, warmup = 500, seed = 1234,
    chains = 2, backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2, refresh = 0)
)

mc <- withr::with_seed(
  seed = 1234, {
    marginalcoef(mpois, CI = 0.95)
  })

test_that("marginalcoef works to integrate out random effects for marginal coefficients in multilevel poisson models", {
  expect_type(mc, "list")
  expect_true(abs(mc$Summary$M[1] - res.samp$Est[1]) < .15)
  expect_true(abs(mc$Summary$M[2] - res.samp$Est[2]) < .05)
})
