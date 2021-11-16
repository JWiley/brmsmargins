<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/JWiley/brmsmargins/branch/main/graph/badge.svg)](https://codecov.io/gh/JWiley/brmsmargins?branch=main)
<!-- badges: end -->

This package has functions to calculate Average Marginal Effects (AMEs)
from `brms` models ( http://paul-buerkner.github.io/brms/ ).

Currently the package only works correctly for models without random effects.

Preliminary, low level code has been added to integrate over random effects 
that are (multivariate) normal, but this has not yet made its way into a 
front end interface.
