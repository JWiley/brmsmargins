test_that(".percent returns NA values when window is NULL", {
  x <- brmsmargins:::.percent(1:10, window = NULL)

  ## check types
  expect_type(x, "list")
  expect_type(x$Window, "double")
  expect_type(x$Percent, "double")
  expect_type(x$Label, "character")

  ## check values
  expect_equal(x$Window, NA_real_)
  expect_equal(x$Percent, NA_real_)
  expect_equal(x$Label, NA_character_)
})

test_that(".percent returns NA values when within is TRUE", {
  x <- brmsmargins:::.percent(1:10, window = c(3, 5))

  ## check types
  expect_type(x, "list")
  expect_type(x$Window, "double")
  expect_type(x$Percent, "double")
  expect_type(x$Label, "character")

  ## check values
  expect_equal(x$Window, c(3, 5))
  expect_equal(x$Percent, 30)
  expect_equal(x$Label, "[3, 5]")
})

test_that(".percent returns NA values when within is FALSE", {
  x <- brmsmargins:::.percent(1:10, window = c(2, 6), within = FALSE)

  ## check types
  expect_type(x, "list")
  expect_type(x$Window, "double")
  expect_type(x$Percent, "double")
  expect_type(x$Label, "character")

  ## check values
  expect_equal(x$Window, c(2, 6))
  expect_equal(x$Percent, 70)
  expect_equal(x$Label, "[-Inf, 2] | [6, Inf]")
})

test_that(".percent errors if window is not valid", {
  expect_error(brmsmargins:::.percent(1:10, window = c(2)))
  expect_error(brmsmargins:::.percent(1:10, window = c("b", "c")))
})
