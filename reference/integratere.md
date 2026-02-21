# Integrate over Random Effects

Used to conduct Monte Carlo integration over Gaussian random effects.
Not intended to be called directly by most users.

## Usage

``` r
integratere(d, sd, L, k, yhat, backtrans)

integratereR(d, sd, L, k, yhat, backtrans)
```

## Arguments

- d:

  A list with model matrices for each random effect block.

- sd:

  A list with standard deviation matrices for each random effect block
  where rows are different posterior draws.

- L:

  A list with matrices for each random effect block containing the parts
  of the L matrix, the Cholesky decomposition of the random effect
  correlation matrix.

- k:

  An integer, the number of samples for Monte Carlo integration.

- yhat:

  A matrix of the fixed effects predictions

- backtrans:

  An integer, indicating the type of back transformation. 0 indicates
  inverse logit (e.g., for logistic regression). 1 indicates exponential
  (e.g., for poisson or negative binomial regression or if outcome was
  natural log transformed). 2 indicates square (e.g., if outcome was
  square root transformed). 3 indicates inverse (e.g., if outcome was
  inverse transformed such as Gamma regression) Any other integer
  results in no transformation. -9 is recommended as the option for no
  transformation as any future transformations supported will be other,
  positive integers.

## Value

A numeric matrix with the Monte Carlo integral calculated.

## Functions

- `integratereR()`: Pure `R` implementation of `integratere()`.

## Examples

``` r
integratere(
  d = list(matrix(1, 1, 1)),
  sd = list(matrix(1, 2, 1)),
  L = list(matrix(1, 2, 1)),
  k = 10L,
  yhat = matrix(0, 2, 1),
  backtrans = 0L)
#>           [,1]
#> [1,] 0.4980764
#> [2,] 0.6267861
```
