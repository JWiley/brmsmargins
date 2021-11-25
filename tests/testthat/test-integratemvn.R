test_that("integratemvn works with a seed", {
  d <- matrix(1, 1, 2)
  sd <- c(10, 5)
  L <- chol(matrix(c(1, .5, .5, 1), 2))

  res1 <- withr::with_seed(
    seed = 1234,
    code = integratemvn(d, 5L, sd, L))

  res2 <- withr::with_seed(
    seed = 1234,
    code = integratemvn(d, 5L, sd, L))

  res3 <- withr::with_seed(
    seed = 4321,
    code = integratemvn(d, 5L, sd, L))

  expect_equal(res1, res2)
  expect_false(isTRUE(all.equal(res1, res3)))
})

test_that("integratemvn works with 0 values", {
  d <- matrix(0, 1, 1)
  sd <- c(1)
  L <- chol(matrix(c(1)))

  res <- withr::with_seed(
    seed = 1234,
    code = integratemvn(d, 5L, sd, L))

  expect_equal(res, matrix(0, 1, 5L))
})

test_that("integratemvn works", {
  d <- matrix(1, 1, 1)
  sd <- c(1)
  L <- chol(matrix(c(1)))

  res <- withr::with_seed(
    seed = 1234,
    code = integratemvn(d, 5000L, sd, L))

  expect_true(abs(rowMeans(res)) < .15)
  expect_true(abs(sd(res[1, ]) - 1) < .15)
})
