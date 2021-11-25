test_that("tab2mat converts row vector to matrix", {
  x <- matrix(1:9, 1)
  res <- tab2mat(x)
  expect_equal(res, matrix(1:9, 3, byrow = TRUE))
})
