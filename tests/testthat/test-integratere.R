test_that("integratere dispatches to normal and Student-t integrations", {
  d <- list(
    diag(2),
    matrix(c(0.5, 1.5), nrow = 2)
  )
  sd <- list(
    matrix(c(1, 2), nrow = 1),
    matrix(c(0.75), nrow = 1)
  )
  L <- list(
    matrix(c(1, 0, 0.25, sqrt(1 - 0.25^2)), nrow = 1),
    matrix(1, nrow = 1)
  )
  df <- list(
    NULL,
    matrix(5, nrow = 1)
  )
  yhat <- matrix(c(-0.5, 0.25), nrow = 1)
  k <- 8L

  manual <- withr::with_seed(1234, {
    z1 <- integratemvn(d[[1]], k, sd[[1]][1, ], tab2mat(L[[1]][1, , drop = FALSE]))
    z2 <- integratemvt(d[[2]], k, sd[[2]][1, ], tab2mat(L[[2]][1, , drop = FALSE]), df[[2]][1])
    zall <- z1 + z2
    zall <- sweep(zall, 1, yhat[1, ], "+")
    zall <- exp(zall)
    matrix(rowMeans(zall), nrow = 1)
  })

  res <- withr::with_seed(
    1234,
    integratere(d = d, sd = sd, L = L, k = k, df = df, yhat = yhat, backtrans = 1L)
  )

  expect_equal(res, manual)
})

test_that("integratere handles Gaussian-only blocks with NULL df entries", {
  d <- list(matrix(c(1, 0), nrow = 1))
  sd <- list(matrix(c(2, 3), nrow = 1))
  L <- list(matrix(c(1, 0, 0, 1), nrow = 1))
  df <- list(NULL)
  yhat <- matrix(0, nrow = 1, ncol = 1)

  res <- withr::with_seed(
    1234,
    integratere(d = d, sd = sd, L = L, k = 5L, df = df, yhat = yhat, backtrans = -9L)
  )

  expect_equal(dim(res), c(1L, 1L))
  expect_true(is.numeric(res))
})
