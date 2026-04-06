skip_on_cran()

backend <- "rstan"
if (requireNamespace("cmdstanr", quietly = TRUE)) {
  if (isFALSE(is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE)))) {
    backend <- "cmdstanr"
  }
} else {
  ## if using rstan backend, models can crash on Windows
  skip_on_os("windows")
}

d_sigma <- withr::with_seed(
  seed = 12345, code = {
    n_id <- 12L
    n_rep <- 4L
    d <- data.table::data.table(
      ID = rep(seq_len(n_id), each = n_rep),
      x = rep(c(0, 1, 0, 1), times = n_id),
      z = rep(c(0, 0, 1, 1), times = n_id))
    d[, y := rnorm(
      .N,
      mean = 1 + 0.75 * x,
      sd = exp(-0.5 + 0.4 * z))]
    data.table::copy(d)
  })

d_mixed <- withr::with_seed(
  seed = 23456, code = {
    n_groups <- 12L
    n_obs <- 6L
    u0 <- rnorm(n_groups, sd = 0.6)
    u1 <- rnorm(n_groups, sd = 0.3)
    d <- data.table::data.table(
      ID = rep(seq_len(n_groups), each = n_obs),
      x = rep(rep(c(0, 1), each = n_obs / 2), times = n_groups))
    d[, y := 1 + 0.8 * x + u0[ID] + u1[ID] * x + rnorm(.N, sd = 0.5)]
    data.table::copy(d)
  })

d_sigma_re <- withr::with_seed(
  seed = 24680, code = {
    n_id <- 10L
    n_rep <- 4L
    u_mu <- rnorm(n_id, sd = 0.4)
    u_sigma <- rnorm(n_id, sd = 0.15)
    d <- data.table::data.table(
      ID = rep(seq_len(n_id), each = n_rep),
      x = rep(c(0, 1, 0, 1), times = n_id),
      z = rep(c(0, 0, 1, 1), times = n_id))
    d[, y := rnorm(
      .N,
      mean = 1 + 0.5 * x + u_mu[ID],
      sd = exp(-0.4 + 0.25 * z + u_sigma[ID]))]
    data.table::copy(d)
  })

d_binom <- withr::with_seed(
  seed = 13579, code = {
    d <- data.table::data.table(
      x = rep(0:1, each = 20L),
      size = sample(4:8, size = 40L, replace = TRUE))
    d[, y := rbinom(.N, size = size, prob = plogis(-0.5 + 1.1 * x))]
    data.table::copy(d)
  })

d_probit <- withr::with_seed(
  seed = 34567, code = {
    d <- data.table::data.table(x = rep(0:1, each = 30L))
    d[, y := rbinom(.N, size = 1, prob = pnorm(-0.2 + 0.8 * x))]
    data.table::copy(d)
  })

d_weibull <- withr::with_seed(
  seed = 45678, code = {
    d <- data.table::data.table(x = rep(0:1, each = 30L))
    d[, y := rweibull(.N, shape = 2, scale = exp(0.2 + 0.3 * x))]
    data.table::copy(d)
  })

suppressWarnings(
  m_sigma <- brms::brm(
    brms::bf(y ~ x, sigma ~ z),
    family = "gaussian",
    data = d_sigma,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

suppressWarnings(
  m_mixed <- brms::brm(
    y ~ 1 + x + (1 + x | ID),
    family = "gaussian",
    data = d_mixed,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

suppressWarnings(
  m_sigma_re <- brms::brm(
    brms::bf(y ~ x + (1 | ID), sigma ~ z + (1 | ID)),
    family = "gaussian",
    data = d_sigma_re,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

suppressWarnings(
  m_binom <- brms::brm(
    y | trials(size) ~ x,
    family = "binomial",
    data = d_binom,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

suppressWarnings(
  m_probit <- brms::brm(
    y ~ x,
    family = brms::bernoulli(link = "probit"),
    data = d_probit,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

suppressWarnings(
  m_weibull <- brms::brm(
    y ~ x,
    family = brms::weibull(),
    data = d_weibull,
    iter = 400,
    warmup = 200,
    chains = 1,
    seed = 1234,
    backend = backend,
    save_pars = brms::save_pars(all = TRUE),
    silent = 2,
    refresh = 0)
)

test_that("assertion and link helpers work with real brms models", {
  expect_error(
    brmsmargins:::.assertbrmsfit(1:3),
    "object must be of class 'brmsfit'")
  expect_true(isTRUE(brmsmargins:::.assertbrmsfit(m_sigma)))

  expect_true(isTRUE(brmsmargins:::.assertfamily(m_sigma)))
  expect_true(isTRUE(brmsmargins:::.assertdpar(m_sigma, NULL)))
  expect_true(isTRUE(brmsmargins:::.assertdpar(m_sigma, "sigma")))
  expect_true(isTRUE(brmsmargins:::.assertRE(m_sigma)))
  expect_equal(brmsmargins:::.extractlink(m_sigma, NULL), "identity")
  expect_equal(brmsmargins:::.extractlink(m_sigma, "sigma"), "log")
  expect_true(isTRUE(brmsmargins:::.assertlink(m_sigma)))
  expect_true(isTRUE(brmsmargins:::.assertlink(m_sigma, "sigma")))

  bad_family_length <- m_sigma
  bad_family_length$family$family <- c("gaussian", "poisson")
  expect_error(brmsmargins:::.assertfamily(bad_family_length), "length 2")

  bad_family_class <- m_sigma
  bad_family_class$family$family <- 1
  expect_error(brmsmargins:::.assertfamily(bad_family_class), "character string")

  expect_error(brmsmargins:::.assertdpar(m_sigma, c("mu", "sigma")), "length 2")
  expect_error(brmsmargins:::.assertdpar(m_sigma, 1), "class character")
  expect_error(brmsmargins:::.assertdpar(m_sigma, "phi"), "was not found")

  bad_link_length <- m_sigma
  bad_link_length$family$link <- c("identity", "log")
  expect_error(brmsmargins:::.assertlink(bad_link_length), "length 2")

  bad_link_class <- m_sigma
  bad_link_class$family$link <- 1
  expect_error(brmsmargins:::.assertlink(bad_link_class), "character string")

  bad_re_dist <- m_mixed
  bad_re_dist$ranef$dist[] <- "lognormal"
  expect_error(brmsmargins:::.assertRE(bad_re_dist), "student-t random effects are supported")

  bad_re_dpar <- m_sigma_re
  bad_re_dpar$ranef$dist[bad_re_dpar$ranef$dpar == "sigma"] <- "student"
  expect_error(brmsmargins:::.assertRE(bad_re_dpar), "only gaussian random effects are supported for dpar")

  expect_error(
    brmsmargins:::.assertlink(m_probit),
    "must be one of")
  expect_error(
    brmsmargins:::.assertfamily(m_weibull),
    "must be one of")
})

test_that("random effect and builder helpers work with real mixed brms models", {
  post <- data.table::as.data.table(posterior::as_draws_df(m_mixed))
  re <- data.table::as.data.table(m_mixed$ranef)
  block <- unique(re$id)[1]
  usere <- re[id == block]
  number <- max(usere$cn)
  sdata <- brms::standata(
    m_mixed,
    newdata = d_mixed[1:4],
    check_response = FALSE,
    allow_new_levels = TRUE)

  expect_true(brmsmargins:::is.random(m_mixed))
  expect_true(isTRUE(brmsmargins:::.assertRE(m_mixed)))

  expect_equal(
    colnames(brmsmargins:::.buildL(post, block = block, number = number, dpar = NULL)),
    brmsmargins:::.namesL(block = block, number = number))
  expect_equal(
    colnames(brmsmargins:::.buildSD(post, ranef = usere, block = block, dpar = NULL)),
    brmsmargins:::.namesSD(ranef = usere, block = block, dpar = NULL))
  expect_equal(
    colnames(brmsmargins:::.buildZ(sdata, block = block, number = number, dpar = NULL)),
    brmsmargins:::.namesZ(block = block, number = number, dpar = NULL))

  post_sigma <- data.table::as.data.table(posterior::as_draws_df(m_sigma_re))
  re_sigma <- data.table::as.data.table(m_sigma_re$ranef)
  block_sigma <- unique(re_sigma[dpar == "sigma", id])[1]
  usere_sigma <- re_sigma[id == block_sigma & dpar == "sigma"]
  number_sigma <- max(usere_sigma$cn)
  sdata_sigma <- brms::standata(
    m_sigma_re,
    newdata = d_sigma_re[1:4],
    check_response = FALSE,
    allow_new_levels = TRUE)

  expect_equal(
    colnames(brmsmargins:::.buildSD(post_sigma, ranef = usere_sigma, block = block_sigma, dpar = "sigma")),
    brmsmargins:::.namesSD(ranef = usere_sigma, block = block_sigma, dpar = "sigma"))
  expect_equal(
    colnames(brmsmargins:::.buildZ(sdata_sigma, block = block_sigma, number = number_sigma, dpar = "sigma")),
    brmsmargins:::.namesZ(block = block_sigma, number = number_sigma, dpar = "sigma"))
})

test_that("pure R low-level integration helpers are reproducible", {
  d_multi <- matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  sd_multi <- c(1.5, 0.5)
  L_multi <- chol(matrix(c(1, 0.25, 0.25, 1), nrow = 2))

  mvn1 <- withr::with_seed(
    101,
    brmsmargins:::integratemvnR(d_multi, 8L, sd_multi, L_multi))
  mvn2 <- withr::with_seed(
    101,
    brmsmargins:::integratemvnR(d_multi, 8L, sd_multi, L_multi))
  mvn3 <- withr::with_seed(
    102,
    brmsmargins:::integratemvnR(d_multi, 8L, sd_multi, L_multi))

  expect_equal(mvn1, mvn2)
  expect_equal(dim(mvn1), c(2L, 8L))
  expect_false(isTRUE(all.equal(mvn1, mvn3)))

  expect_equal(
    withr::with_seed(
      103,
      brmsmargins:::integratemvnR(matrix(0, 1, 1), 5L, c(1), matrix(1, 1, 1))),
    matrix(0, 1, 5L))

  mvt1 <- withr::with_seed(
    201,
    brmsmargins:::integratemvtR(d_multi, 8L, sd_multi, L_multi, df = 6))
  mvt2 <- withr::with_seed(
    201,
    brmsmargins:::integratemvtR(d_multi, 8L, sd_multi, L_multi, df = 6))
  mvt3 <- withr::with_seed(
    202,
    brmsmargins:::integratemvtR(d_multi, 8L, sd_multi, L_multi, df = 6))

  expect_equal(mvt1, mvt2)
  expect_equal(dim(mvt1), c(2L, 8L))
  expect_false(isTRUE(all.equal(mvt1, mvt3)))

  expect_equal(
    withr::with_seed(
      203,
      brmsmargins:::integratemvtR(matrix(0, 1, 1), 5L, c(1), matrix(1, 1, 1), df = 5)),
    matrix(0, 1, 5L))
})

test_that("pure R tab2mat helper reconstructs square matrices", {
  expect_equal(
    brmsmargins:::tab2matR(matrix(c(1, 2, 3, 4), nrow = 1)),
    matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  expect_equal(
    brmsmargins:::tab2matR(matrix(5, nrow = 1)),
    matrix(5, nrow = 1))
})

test_that("pure R random effect integration handles all back transformations", {
  d <- list(
    diag(2),
    matrix(c(0.5, 1.5), nrow = 2))
  sd <- list(
    matrix(c(1, 2), nrow = 1),
    matrix(c(0.75), nrow = 1))
  L <- list(
    matrix(c(1, 0, 0.25, sqrt(1 - 0.25^2)), nrow = 1),
    matrix(1, nrow = 1))
  df <- list(
    NULL,
    matrix(5, nrow = 1))
  yhat <- matrix(c(-0.5, 0.25), nrow = 1)
  k <- 8L

  identity_res <- withr::with_seed(
    301,
    brmsmargins:::integratereR(
      d = d,
      sd = sd,
      L = L,
      k = k,
      df = df,
      yhat = yhat,
      backtrans = -9L))
  logistic_res <- withr::with_seed(
    302,
    brmsmargins:::integratereR(
      d = d,
      sd = sd,
      L = L,
      k = k,
      df = df,
      yhat = yhat,
      backtrans = 0L))
  exp_res <- withr::with_seed(
    303,
    brmsmargins:::integratereR(
      d = d,
      sd = sd,
      L = L,
      k = k,
      df = df,
      yhat = yhat,
      backtrans = 1L))
  square_res <- withr::with_seed(
    304,
    brmsmargins:::integratereR(
      d = d,
      sd = sd,
      L = L,
      k = k,
      df = df,
      yhat = yhat,
      backtrans = 2L))

  expect_equal(dim(identity_res), c(1L, 2L))
  expect_true(any(identity_res < 0))
  expect_true(all(logistic_res > 0 & logistic_res < 1))
  expect_true(all(exp_res > 0))
  expect_true(all(square_res >= 0))
})

test_that("average posterior and prediction branches work with real brms models", {
  posterior <- matrix(1:12, nrow = 3, byrow = TRUE)

  expect_equal(
    brmsmargins:::.averagePosterior(posterior),
    rowMeans(posterior))

  boot1 <- brmsmargins:::.averagePosterior(posterior, resample = 4L, seed = 123)
  boot2 <- brmsmargins:::.averagePosterior(posterior, resample = 4L, seed = 123)
  expect_equal(boot1, boot2)
  expect_length(boot1, nrow(posterior) * 4L)

  resample1 <- brmsmargins::prediction(
    m_sigma,
    data = d_sigma[1:4],
    summarize = FALSE,
    posterior = TRUE,
    dpar = "sigma",
    resample = 3L,
    resampleseed = 456)
  resample2 <- brmsmargins::prediction(
    m_sigma,
    data = d_sigma[1:4],
    summarize = FALSE,
    posterior = TRUE,
    dpar = "sigma",
    resample = 3L,
    resampleseed = 456)

  expect_equal(resample1$Posterior, resample2$Posterior)
  expect_length(resample1$Posterior, posterior::ndraws(m_sigma) * 3L)

  raw <- NULL
  expect_message(
    expect_message(
      raw <- brmsmargins::prediction(
        m_mixed,
        data = d_mixed[1:4],
        effects = "includeRE",
        raw = TRUE,
        summarize = TRUE,
        posterior = FALSE),
      "summarize cannot be TRUE when raw = TRUE"),
    "posterior cannot be FALSE when raw = TRUE")

  expect_null(raw$Summary)
  expect_equal(dim(raw$Posterior), c(posterior::ndraws(m_mixed), 4L))

  expect_error(
    brmsmargins::prediction(m_sigma, data = d_sigma[1:4], effects = "includeRE"),
    "must use \"effects = 'fixedonly'\"")

  sigma_re <- brmsmargins::prediction(
    m_sigma_re,
    data = d_sigma_re[1:4],
    dpar = "sigma",
    effects = "integrateoutRE",
    k = 20L,
    summarize = FALSE,
    posterior = TRUE)
  expect_equal(length(sigma_re$Posterior), posterior::ndraws(m_sigma_re))
  expect_true(all(sigma_re$Posterior > 0))

  one_row <- d_binom[1]
  prob_draws <- drop(
    fitted(
      m_binom,
      newdata = one_row,
      re_formula = NA,
      scale = "response",
      summary = FALSE))
  count_pred <- brmsmargins::prediction(
    m_binom,
    data = one_row,
    summarize = FALSE,
    posterior = TRUE)

  expect_equal(count_pred$Posterior, prob_draws * one_row$size)
  expect_true(all(count_pred$Posterior >= 0))
  expect_error(
    brmsmargins::prediction(
      m_binom,
      data = subset(one_row, select = -size),
      summarize = FALSE,
      posterior = TRUE),
    "size")
})

test_that("brmsmargins and marginalcoef work with real brms models", {
  use_data <- data.table::copy(d_sigma[ID <= 6])

  manual0 <- data.table::copy(use_data)
  manual0[, x := 0]
  manual1 <- data.table::copy(use_data)
  manual1[, x := 1]

  pred0 <- brmsmargins::prediction(
    m_sigma,
    data = manual0,
    posterior = TRUE,
    CI = 0.8)
  pred1 <- brmsmargins::prediction(
    m_sigma,
    data = manual1,
    posterior = TRUE,
    CI = 0.8)

  at_res <- brmsmargins::brmsmargins(
    m_sigma,
    at = data.frame(x = c(0, 1)),
    newdata = use_data,
    subset = "ID <= 6",
    CI = 0.8,
    contrasts = matrix(c(-1, 1), nrow = 2))

  expect_equal(at_res$Posterior[, 1], pred0$Posterior)
  expect_equal(at_res$Posterior[, 2], pred1$Posterior)
  expect_equal(at_res$ContrastSummary$Label, "Contrast_1")

  add_res <- NULL
  expect_message(
    add_res <- brmsmargins::brmsmargins(
      m_sigma,
      add = data.frame(x = c(0, 0.1)),
      newdata = use_data,
      CI = 0.8,
      verbose = TRUE),
    "It is unusual to specify 'add' without 'contrasts'")

  expect_identical(add_res$Contrasts, NA)
  expect_identical(add_res$ContrastSummary, NA)

  wat <- list(
    ID = "ID",
    x = data.frame(
      ID = sort(unique(use_data$ID)),
      variable = 1,
      value = seq(0, 1, length.out = data.table::uniqueN(use_data$ID))))

  manual_wat <- data.table::copy(use_data)
  for (useid in unique(manual_wat$ID)) {
    manual_wat[ID == useid, x := wat$x$value[wat$x$ID == useid]]
  }
  wat_pred <- brmsmargins::prediction(
    m_sigma,
    data = manual_wat,
    posterior = TRUE,
    CI = 0.8)

  wat_res <- brmsmargins::brmsmargins(
    m_sigma,
    at = data.frame(x = 1),
    wat = wat,
    newdata = use_data,
    CI = 0.8)

  expect_equal(wat_res$Posterior[, 1], wat_pred$Posterior)

  xmat <- brms::make_standata(stats::formula(m_mixed), data = model.frame(m_mixed))$X
  mc <- brmsmargins::marginalcoef(m_mixed, posterior = TRUE, k = 20L, CI = 0.8)
  mc_seed1 <- brmsmargins::marginalcoef(m_mixed, posterior = TRUE, k = 20L, CI = 0.8, seed = 321)
  mc_seed2 <- brmsmargins::marginalcoef(m_mixed, posterior = TRUE, k = 20L, CI = 0.8, seed = 321)
  lm_fit <- stats::coef(stats::lm(y ~ x, data = d_mixed))

  expect_equal(mc$Summary$Label, colnames(xmat))
  expect_equal(dim(mc$Posterior), c(ncol(xmat), posterior::ndraws(m_mixed)))
  expect_true(all(abs(mc$Summary$M - lm_fit) < 0.5))
  expect_equal(mc_seed1$Posterior, mc_seed2$Posterior)
  expect_error(
    brmsmargins::marginalcoef(m_mixed, posterior = TRUE, k = 20L, CI = 0.8, seed = 1:2))
  expect_error(
    brmsmargins::marginalcoef(m_sigma, k = 20L),
    "object must have random effects to use marginalcoef")

  at_seed1 <- brmsmargins::brmsmargins(
    m_mixed,
    at = data.frame(x = c(0, 1)),
    newdata = d_mixed[1:12],
    seed = c(11, 22),
    CI = 0.8)
  at_seed2 <- brmsmargins::brmsmargins(
    m_mixed,
    at = data.frame(x = c(0, 1)),
    newdata = d_mixed[1:12],
    seed = c(11, 22),
    CI = 0.8)
  add_seed1 <- brmsmargins::brmsmargins(
    m_mixed,
    add = data.frame(x = c(0, 0.1)),
    newdata = d_mixed[1:12],
    seed = c(33, 44),
    CI = 0.8)
  add_seed2 <- brmsmargins::brmsmargins(
    m_mixed,
    add = data.frame(x = c(0, 0.1)),
    newdata = d_mixed[1:12],
    seed = c(33, 44),
    CI = 0.8)

  expect_equal(at_seed1$Posterior, at_seed2$Posterior)
  expect_equal(add_seed1$Posterior, add_seed2$Posterior)
  expect_error(
    brmsmargins::brmsmargins(
      m_mixed,
      at = data.frame(x = c(0, 1)),
      newdata = d_mixed[1:12],
      seed = 1:3,
      CI = 0.8))
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      at = data.frame(x = c(0, 1)),
      newdata = 1:4,
      CI = 0.8),
    "newdata:")
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      at = 1:2,
      newdata = use_data,
      CI = 0.8),
    "at:")
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      add = 1:2,
      newdata = use_data,
      CI = 0.8),
    "add:")
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      at = data.frame(x = c(0, 1)),
      contrasts = 1:2,
      newdata = use_data,
      CI = 0.8),
    "contrasts:")
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      at = data.frame(x = c(0, 1)),
      newdata = use_data,
      CI = 1.5),
    "'CI' is")
  expect_error(
    brmsmargins::brmsmargins(
      m_sigma,
      at = data.frame(x = c(0, 1)),
      newdata = use_data,
      CI = 0.8,
      CIType = "BAD"),
    "'CIType' is")
})
