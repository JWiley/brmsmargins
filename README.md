<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/JWiley/brmsmargins/branch/main/graph/badge.svg)](https://codecov.io/gh/JWiley/brmsmargins?branch=main)
<!-- badges: end -->

This package has functions to calculate Average Marginal Effects (AMEs)
from `brms` models ( http://paul-buerkner.github.io/brms/ ).

Preliminary work has been done to implement integration over random effects 
that are (multivariate) normal to allow correct AMEs for models with 
fixed and random effects. Use this at your own risk as testing and evaluation 
is currently underway.
