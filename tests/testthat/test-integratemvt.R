test_that("integratemvt works with a seed", {
  d <- matrix(1, 1, 2)
  sd <- c(10, 5)
  L <- chol(matrix(c(1, .5, .5, 1), 2))

  res1 <- withr::with_seed(
    seed = 1234,
    code = integratemvt(d, 5L, sd, L, df = 5))

  res2 <- withr::with_seed(
    seed = 1234,
    code = integratemvt(d, 5L, sd, L, df = 5))

  res3 <- withr::with_seed(
    seed = 4321,
    code = integratemvt(d, 5L, sd, L, df = 5))

  expect_equal(res1, res2)
  expect_false(isTRUE(all.equal(res1, res3)))
})

test_that("integratemvt works with 0 values", {
  d <- matrix(0, 1, 1)
  sd <- c(1)
  L <- chol(matrix(c(1)))

  res <- withr::with_seed(
    seed = 1234,
    code = integratemvt(d, 5L, sd, L, df = 5))

  expect_equal(res, matrix(0, 1, 5L))
})

test_that("integratemvt works", {
  d <- matrix(1, 1, 1)
  sd <- c(1)
  L <- chol(matrix(c(1)))
  df <- 5

  res <- withr::with_seed(
    seed = 1234,
    code = integratemvt(d, 5000L, sd, L, df = df))

  expect_true(abs(rowMeans(res)) < .15)
  expect_true(abs(sd(res[1, ]) - sqrt(df / (df - 2))) < .2)
})

test_that("integratemvt and integratemvn are similar with large df", {
  d <- matrix(1, 1, 1)
  sd <- c(1)
  L <- chol(matrix(c(1)))

  res1 <- withr::with_seed(
    seed = 12345,
    code = integratemvn(d, 5000L, sd, L))

  res2 <- withr::with_seed(
    seed = 12345,
    code = integratemvt(d, 5000L, sd, L, df = 1))

  res3 <- withr::with_seed(
    seed = 12345,
    code = integratemvt(d, 5000L, sd, L, df = 500))

  f <- ecdf(res1[1, ])

  dens2 <- f(res2[1, ])
  du2 <- density(dens2, from = .05, to = .95)

  dens2 <- f(res3[1, ])
  du3 <- density(dens2, from = .05, to = .95)

  mu2 <- mean(du2$y)
  mu3 <- mean(du3$y)

  ## relative density should be lower for the t with low df than high df relative to normal
  expect_true(mu2 < mu3)
  ## the t with low df should have more density in the tails than the normal
  ## so relative density should be lower in the middle
  expect_true(mu2 < .90)
  ## the t with high df should be similar to the normal,
  ## so relative density should be close to 1 in the middle
  expect_true(mu3 > .96)
})
