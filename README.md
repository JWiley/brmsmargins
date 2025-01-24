<!-- badges: start -->
[![CRAN-status](https://www.r-pkg.org/badges/version/brmsmargins)](https://cran.r-project.org/package=brmsmargins)
[![R-CMD-check](https://github.com/JWiley/brmsmargins/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JWiley/brmsmargins/actions)
[![codecov](https://codecov.io/gh/JWiley/brmsmargins/graph/badge.svg?token=VXf0Qo0PRY)](https://codecov.io/gh/JWiley/brmsmargins)
[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

This package has functions to calculate marginal effects 
from `brms` models ( http://paul-buerkner.github.io/brms/ ).
A central motivator is to calculate average marginal effects (AMEs)
for continuous and discrete predictors in fixed effects only and 
mixed effects regression models including location scale models.

This table shows an overview of currently supported 
models / features where "X" indicates a specific model / feature 
is currently supported. The column 'Fixed' means fixed effects only models.
The column 'Mixed' means mixed effects models.

| Distribution / Feature                         | Fixed              | Mixed              |
|------------------------------------------------|--------------------|--------------------|
| Gaussian / Normal                              | :heavy_check_mark: | :heavy_check_mark: |
| Bernoulli (logistic)                           | :heavy_check_mark: | :heavy_check_mark: |
| Poisson                                        | :heavy_check_mark: | :heavy_check_mark: |
| Negative Binomial                              | :heavy_check_mark: | :heavy_check_mark: |
| Gamma                                          | :heavy_check_mark: | :heavy_check_mark: |
| Beta                                           | :heavy_check_mark: | :heavy_check_mark: |
| Multinomial logistic                           | :x:                | :x:                |
| Multivariate models                            | :x:                | :x:                |
| Gaussian location scale models                 | :heavy_check_mark: | :heavy_check_mark: |
| Natural log / square root transformed outcomes | :heavy_check_mark: | :heavy_check_mark: |
| Monotonic predictors                           | :heavy_check_mark: | :heavy_check_mark: |
| Custom outcome transformations                 | :x:                | :x:                |

In general, any distribution supported by `brms` that generates one and 
only one predicted value (e.g., not multinomial logistic regression models)
should be supported for fixed effects only models.
Also note that currently, only Gaussian random effects are supported. This is not too 
limiting as even for Bernoulli, Poisson, etc. outcomes, the random effects 
are commonly assumed to have a Gaussian distribution.

Here is a quick syntax overview of how to use the main function,
`brmsmargins()`. 

#### Fixed effects, continuous predictor.

```r
h <- .001
ames <- brmsmargins(
  object = model,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "fixedonly")
  
ames$ContrastSummary
```

#### Fixed effects, discrete predictor.

```r
ames <- brmsmargins(
  object = model,
  add = data.frame(x = c(0, 1)),
  contrasts = cbind("AME x" = c(-1, 1)),
  effects = "fixedonly")

ames$Summary
ames$ContrastSummary
```

#### Mixed effects, continuous predictor.

```r
h <- .001
ames <- brmsmargins(
  object = model,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  effects = "integrateoutRE")
  
ames$ContrastSummary
```

#### Mixed effects, discrete predictor.

```r
ames <- brmsmargins(
  object = model,
  add = data.frame(x = c(0, 1)),
  contrasts = cbind("AME x" = c(-1, 1)),
  effects = "integrateoutRE")

ames$Summary
ames$ContrastSummary
```

#### Mixed Effects Location Scale, continuous predictor

```r
h <- .001
ames <- brmsmargins(
  object = model,
  add = data.frame(x = c(0, h)),
  contrasts = cbind("AME x" = c(-1 / h, 1 / h)),
  dpar = "sigma",
  effects = "integrateoutRE")
  
ames$ContrastSummary
```

#### Mixed Effects Location Scale, discrete predictor

```r
ames <- brmsmargins(
  object = model,
  at = data.frame(x = c(0, 1)),
  contrasts = cbind("AME x" = c(-1, 1)),
  dpar = "sigma",
  effects = "integrateoutRE")

ames$Summary
ames$ContrastSummary
```

Note that even on mixed effects models, it is possible to generate 
predictions and marginal effects from the fixed effects only, 
just by specifying `effects = "fixedonly"` but this is 
probably not a good idea generally so not shown by default.

Also note that for all of these examples `ames$Summary` would 
have a summary of the averaged predicted values. These often 
are useful for discrete predictors. For continuous 
predictors, if the focus is on marginal effects, they often are
not interesting. However, the `at` argument can be used 
with continuous predictors to generate interesting averaged
predicted values. For example, this would get predicted 
values integrating out random effects for a range of ages
averaging (marginalizing) all other predictors / covariates.

```r
ames <- brmsmargins(
  object = model,
  at = data.frame(age = c(20, 30, 40, 50, 60)),
  effects = "integrateoutRE")

ames$Summary
```

## Installation

You can install the package from CRAN by running this code:

```r
install.packages("brmsmargins")
```

Alternately, for the latest, development version, run:

```r
remotes::install_github("JWiley/brmsmargins")
```

## Learn More

There are three vignettes that introduce how to use the package
for several scenarios.

- [Fixed effects only models](https://joshuawiley.com/brmsmargins/articles/fixed-effects-marginaleffects.html)
  (also called single level models). This also is the best place to start learning 
  about how to use the package. It includes a brief amount of motivation for 
  why we would want to calculate marginal effects at all.
- [Mixed effects models](https://joshuawiley.com/brmsmargins/articles/mixed-effects-marginaleffects.html)
  (also called multilevel models). This shows how to calculate marginal effects 
  for mixed effects / multilevel models. There are runnable examples, but
  not much background.
- [Location scale models](https://joshuawiley.com/brmsmargins/articles/location-scale-marginaleffects.html).
  Location scale models are models where both the location (e.g., mean) and 
  scale (e.g., variance / residual standard deviation) are explicitly 
  modeled as outcomes. These require use of distributional parameters `dpar` in `brms`.
  This vignette shows how to calculate marginal effects from location scale models for the 
  scale part.
