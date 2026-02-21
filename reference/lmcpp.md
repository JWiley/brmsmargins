# Fast Linear Regression

Used to get marginal coefficients off of a generalized linear mixed
model.

## Usage

``` r
lmcpp(X, y)
```

## Arguments

- X:

  A numeric model matrix. If intercept is desired, it must already have
  been added as a column.

- y:

  A numeric matrix. A single column if one response variable or multiple
  columns where each column is a different response, such as a for
  marginal coefficients where each column is a different MCMC sample.

## Value

A numeric matrix with the coefficient.

## Examples

``` r
lmcpp(cbind(1, mtcars$hp, mtcars$am), as.matrix(mtcars[, c("mpg", "qsec")]))
#>            [,1]        [,2]
#> [1,] 26.5849137 21.57549953
#> [2,] -0.0588878 -0.02116732
#> [3,]  5.2770853 -1.53050633
```
