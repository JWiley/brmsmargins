test_that("rowBootMeans differs from rowMeans and bootstraps in an expected way", {
  x <- matrix(1:9, 3)
  res <- withr::with_seed(
    seed = 1234,
    rowBootMeans(x))
  expect_equal(res, c(3, 6, 4))
  expect_false(all(res == rowMeans(x)))
})
