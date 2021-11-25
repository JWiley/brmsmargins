skip_on_cran()

d <- withr::with_seed(
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
        lambda = exp(theta.location[i, 1] + theta.location[i, 2] * x))
        ]
    }
    copy(d)
  })

res.samp <- d[, .(M = mean(y)), by = .(ID, x)][, .(M = mean(M)), by = x]

mpois <- brms::brm(
  y ~ 1 + x + (1 + x | ID), family = "poisson",
  data = d, iter = 1000, warmup = 500, seed = 1234,
  chains = 2, backend = "cmdstanr",
  silent = 2, refresh = 0)

preddat <- data.frame(y = c(0, 0), x = c(0, 1), ID = 999)

res.integrate <- withr::with_seed(
  seed = 1234, {
    test0 <- .predict(object = mpois, data = preddat[1, ], posterior = TRUE,
                      effects = "integrateoutRE", k = 100L, CI = 0.95, type = "ETI")
    test1 <- .predict(object = mpois, data = preddat[2, ], posterior = TRUE,
                      effects = "integrateoutRE", k = 100L, CI = 0.95, type = "ETI")
    ame <- list(Summary = NULL, Posterior = test1$Posterior - test0$Posterior)
    ame$Summary <- bsummary(ame$Posterior, CI = 0.95, type = "ETI")

    list(
      Summary = rbind(
        test0$Summary, test1$Summary, ame$Summary),
      Posterior = cbind(
        test0$Posterior, test1$Posterior, ame$Posterior))
  })

res.fixedonly <- withr::with_seed(
  seed = 1234, {
    test0 <- .predict(object = mpois, data = preddat[1, ], posterior = TRUE,
                      effects = "fixedonly", CI = 0.95, type = "ETI")
    test1 <- .predict(object = mpois, data = preddat[2, ], posterior = TRUE,
                      effects = "fixedonly", CI = 0.95, type = "ETI")
    ame <- list(Summary = NULL, Posterior = test1$Posterior - test0$Posterior)
    ame$Summary <- bsummary(ame$Posterior, CI = 0.95, type = "ETI")

    list(
      Summary = rbind(
        test0$Summary, test1$Summary, ame$Summary),
      Posterior = cbind(
        test0$Posterior, test1$Posterior, ame$Posterior))
  })

test_that(".predict works to integrate out random effects in multilevel poisson models", {
  expect_type(res.integrate, "list")
  expect_equal(
    c(ndraws(mpois), 3L),
    dim(res.integrate$Posterior))
  expect_true(all(
    res.integrate$Posterior[, 1:2] >= 0))
  expect_true(all(
    res.integrate$Summary$M >= 0))

  expect_true(abs(res.integrate$Summary$M[1] - res.samp$M[1]) < .05)
  expect_true(abs(res.integrate$Summary$M[2] - res.samp$M[2]) < .10)
  expect_true(abs(res.integrate$Summary$M[3] -
                    (res.samp$M[2] - res.samp$M[1])) < .05)
})

test_that(".predict works with fixed effects only in multilevel poisson models", {
  expect_type(res.fixedonly, "list")
  expect_equal(
    c(ndraws(mpois), 3L),
    dim(res.fixedonly$Posterior))
  expect_true(all(
    res.fixedonly$Posterior[, 1:2] >= 0))
  expect_true(all(
    res.fixedonly$Summary$M >= 0))
  expect_true(res.fixedonly$Summary$M[1] < res.integrate$Summary$M[1])
  expect_true(res.fixedonly$Summary$M[2] < res.integrate$Summary$M[2])
})
