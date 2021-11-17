test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


## library(brms)
## library(brmsmargins)

## income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
## income <- factor(sample(income_options, 100, TRUE), 
##                  levels = income_options, ordered = TRUE)
## mean_ls <- c(30, 60, 70, 75)
## ls <- mean_ls[income] + rnorm(100, sd = 7)
## dat <- data.frame(income, ls)

## fit1 <- brm(ls ~ mo(income), data = dat)

## marg <- brmsmargins(
##   fit1,
##   at = data.frame(
##     income = factor(levels(dat$income), ordered = TRUE)),
##   ## sequential, pairwise contrasts
##   contrasts = matrix(c(rep(c(-1, 1, 0, 0, 0), 2),-1, 1), 4))


## ## these should be the same
## conditional_effects(fit1, plot = FALSE)$income

## ##        income       ls cond__   effect1__ estimate__     se__  lower__  upper__
## ## 1    below_20 57.40319      1    below_20   29.46575 1.181903 27.16700 32.00974
## ## 2    20_to_40 57.40319      1    20_to_40   60.45641 1.154384 58.16923 62.78298
## ## 3   40_to_100 57.40319      1   40_to_100   71.00420 1.240089 68.53623 73.33976
## ## 4 greater_100 57.40319      1 greater_100   74.39849 1.364693 71.94436 77.12927

## marg$Summary
## ##           M      Mdn       LL       UL PercentROPE PercentMID   CI CIType ROPE
## ## 1: 29.49498 29.46575 26.71083 32.88775          NA         NA 0.99    HDI <NA>
## ## 2: 60.46622 60.45641 57.47392 63.50555          NA         NA 0.99    HDI <NA>
## ## 3: 70.98177 71.00420 67.80697 74.09095          NA         NA 0.99    HDI <NA>
## ## 4: 74.43314 74.39849 70.99505 77.74676          NA         NA 0.99    HDI <NA>

## ## these give the pairwise contrasts
## marg$ContrastSummary

## ##            M       Mdn         LL        UL PercentROPE PercentMID   CI CIType
## ## 1: 30.971235 30.997266 26.3899437 34.851657          NA         NA 0.99    HDI
## ## 2: 10.515554 10.507766  6.1273547 14.900275          NA         NA 0.99    HDI
## ## 3:  3.451367  3.348807  0.0188492  7.900816          NA         NA 0.99    HDI
## ##    ROPE  MID      Label
## ## 1: <NA> <NA> Contrast_1
## ## 2: <NA> <NA> Contrast_2
## ## 3: <NA> <NA> Contrast_3
