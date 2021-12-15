test_that(".namesL creates proper names that can be converted to a matrix", {
  expect_equal(
    brmsmargins:::.namesL(1, 1),
    "L_1[1,1]")

  expect_equal(
    dim(brmsmargins:::tab2matR(
      matrix(brmsmargins:::.namesL(1, 3), 1))),
    c(3L, 3L))
})

test_that(".namesZ creates proper names", {
  expect_equal(
    brmsmargins:::.namesZ(1, 3, NULL),
    c("Z_1_1", "Z_1_2", "Z_1_3"))
})
