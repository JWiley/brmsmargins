test_that(".checktab returns a non zero character string if invalid input", {
  x <- brmsmargins:::.checktab(1:5)
  expect_type(x, "character")
  expect_true(nzchar(x))
})

test_that(".checktab returns an empty character string if invalid input", {
  x <- brmsmargins:::.checktab(mtcars)
  expect_type(x, "character")
  expect_false(nzchar(x))
})
