# Changelog

## brmsmargins 0.3.0

- Now supports response variables with a binomial distribution.
  Predictions that marginalise over random effects will use the number
  of trials. For contrasts, such as using
  [`brmsmargins()`](https://joshuawiley.com/brmsmargins/reference/brmsmargins.md)
  you can use the `at =` argument to force trials to 1 to get marginal
  probabilities rather than counts of successful trials.
- Experimental support for integrating out random effects from a student
  t distribution. Currently only supports this for location parameters.
  When `dpar` is not empty in `brms` only Gaussian distributions are
  supported. Some caution is needed here as `brms` uses the same degree
  of freedom parameter for all random effects of the same grouping
  variable, so for complex random effect structures, there may be errors
  in how they are integrated out.

## brmsmargins 0.2.1

CRAN release: 2025-09-26

- Added `seed` argument to
  [`marginalcoef()`](https://joshuawiley.com/brmsmargins/reference/marginalcoef.md)
  to allow for reproducible results.
- Minor documentation edits.

## brmsmargins 0.2.0

CRAN release: 2022-05-18

- Fixed a bug when using
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md)
  with option `effects = "integrateoutRE"` when smooth terms were
  present. As
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md)
  underpins other functions, such as
  [`brmsmargins()`](https://joshuawiley.com/brmsmargins/reference/brmsmargins.md)
  this issue also impacts those other functions.
- New function:
  [`marginalcoef()`](https://joshuawiley.com/brmsmargins/reference/marginalcoef.md)
  which calculates population averaged (marginal) coefficients for the
  fixed effects coefficients from mixed effects models using a method
  described by Donald Hedeker, who joins the author team. Currently,
  only the main location parameter is supported. That is, marginal
  coefficients for the scale part of a model, in location and scale
  models, is not currently supported.
- New argument, `wat`, added to
  [`brmsmargins()`](https://joshuawiley.com/brmsmargins/reference/brmsmargins.md)
  to support including calculating average marginal effects for
  multilevel centered categorical predictors.
- Updates to vignettes demonstrating: (1) the use of marginal
  coefficients;
  2.  marginal effects for centered categorical predictors; and
  3.  ‘simple’ marginal effects when models include interaction terms.
- Revised documentation for `bmrsmargins()` and
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md)
  to be clearer around which arguments users must directly specify and
  which are optional or have sensible defaults.
- Added more unit testing and vignettes.

## brmsmargins 0.1.1

CRAN release: 2021-12-16

- Fixed a bug preventing predictions integrating out random effects for
  mixed effects models with a random intercept only (reported in
  Issue#1). Thanks to [@ajnafa](https://github.com/ajnafa) for
  reporting.
- Added support for Gamma and Beta regression models.
- More extensive testing added.

## brmsmargins 0.1.0

- Initial release
