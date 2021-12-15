test_that("bsummary errors if invalid input", {
  expect_error(bsummary(letters[1:5]))
})
  
test_that("bsummary works", {
  x <- bsummary(1:100)

  ## check types
  expect_s3_class(x, "data.table")
  expect_type(x$M, "double")
  expect_type(x$Mdn, "double")
  expect_type(x$LL, "double")
  expect_type(x$UL, "double")
  expect_type(x$PercentROPE, "double")
  expect_type(x$PercentMID, "double")
  expect_type(x$CI, "double")
  expect_type(x$CIType, "character")
  expect_type(x$ROPE, "character")
  expect_type(x$MID, "character")

  ## check values
  expect_equal(x$M, 50.5)
  expect_equal(x$Mdn, 50.5)
  expect_equal(x$LL, 1)
  expect_equal(x$UL, 100)
  expect_equal(x$PercentROPE, NA_real_)
  expect_equal(x$PercentMID, NA_real_)
  expect_equal(x$CI, 0.99)
  expect_equal(x$CIType, "HDI")
  expect_equal(x$ROPE, NA_character_)
  expect_equal(x$MID, NA_character_)
})

test_that("bsummary works with ROPEs and MIDs", {
  x <- bsummary((-50:60) / 100, ROPE = c(-.5, .5), MID = c(-1, 1))

  ## check types
  expect_s3_class(x, "data.table")
  expect_type(x$M, "double")
  expect_type(x$Mdn, "double")
  expect_type(x$LL, "double")
  expect_type(x$UL, "double")
  expect_type(x$PercentROPE, "double")
  expect_type(x$PercentMID, "double")
  expect_type(x$CI, "double")
  expect_type(x$CIType, "character")
  expect_type(x$ROPE, "character")
  expect_type(x$MID, "character")

  ## check values
  expect_equal(x$M, 0.05)
  expect_equal(x$Mdn, 0.05)
  expect_true(x$PercentROPE > 50)
  expect_equal(x$PercentMID, 0)
  expect_equal(x$CI, 0.99)
  expect_equal(x$CIType, "HDI")
  expect_equal(x$ROPE, "[-0.5, 0.5]")
  expect_equal(x$MID, "[-Inf, -1] | [1, Inf]")
})
