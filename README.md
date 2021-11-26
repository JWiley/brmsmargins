<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/JWiley/brmsmargins/branch/main/graph/badge.svg)](https://codecov.io/gh/JWiley/brmsmargins?branch=main)
<!-- badges: end -->

This package has functions to calculate Average Marginal Effects (AMEs)
from `brms` models ( http://paul-buerkner.github.io/brms/ ).

The package is not yet on CRAN, so to install, you must use the 
development version. To install, run:

```

devtools::install_github("JWiley/brmsmargins")

```

The main function is `brmsmargins()`. There is good support for 
single level models. Here is simple example of how it may be run:

```

ames <- brmsmargins(
  object = mlogit,
  at = data.table::data.table(x = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  effects = "fixedonly")

ames$Summary
ames$ContrastSummary

```

Perhaps even more important than the work for single level models,
`brmsmargins()` has some support for AMEs with mixed effects / multilevel models.
Specifically, for models with random effects that are (multivariate) normal,
mixed effects logistic, poisson, and negative binomial models should be supported.
Gaussian models with transformed outcomes (log, square root) are supported.
For these models, `brmsmargins()` should correctly integrate out the random effects,
to give accurate point estimates and credible intervals.

This is very much in development. Small scale simulations have been run 
showing accuracy of the results under select conditions, and some basic 
code tests are implemented. However, consider this alpha/beta features for now.
Here is an example of how it may be run:

```

ames <- brmsmargins(
  object = mlogit,
  at = data.table::data.table(x = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  effects = "integrateoutRE", k = 100L)

```
