test_that(".links returns correct values with identity link and fixedonly", {
  x <- brmsmargins:::.links(
    link = "identity", effects = "fixedonly", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "response")
  expect_equal(x$ilink, "identity")
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, -9)
})

test_that(".links returns correct values with logit link and fixedonly", {
  x <- brmsmargins:::.links(
    link = "logit", effects = "fixedonly", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "response")
  expect_equal(x$ilink, "invlogit")
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, 0L)
  expect_equal(x$useilinknum, -9L)
})

test_that(".links returns correct values with identity link and integrateoutRE", {
  x <- brmsmargins:::.links(
    link = "identity", effects = "integrateoutRE", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "linear")
  expect_equal(x$ilink, "identity")
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, -9)
})

test_that(".links returns correct values with logit link and integrateoutRE", {
  x <- brmsmargins:::.links(
    link = "logit", effects = "integrateoutRE", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "linear")
  expect_equal(x$ilink, "invlogit")
  expect_equal(x$ifun(0.5), plogis(0.5))
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, 0L)
  expect_equal(x$useilinknum, 0L)
})

test_that(".links returns correct values with log link and integrateoutRE", {
  x <- brmsmargins:::.links(
    link = "log", effects = "integrateoutRE", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "linear")
  expect_equal(x$ilink, "exp")
  expect_equal(x$ifun(0.5), exp(0.5))
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, 1L)
  expect_equal(x$useilinknum, 1L)
})

test_that(".links returns correct values with sqrt link and integrateoutRE", {
  x <- brmsmargins:::.links(
    link = "sqrt", effects = "integrateoutRE", backtrans = "response")

  expect_type(x, "list")
  expect_equal(x$scale, "linear")
  expect_equal(x$ilink, "square")
  expect_equal(x$ifun(0.5), (0.5)^2)
  expect_equal(x$useifun(0.5), 0.5)
  expect_equal(x$ilinknum, 2)
  expect_equal(x$useilinknum, 2)
})
