# Bootstrap Row Means

This takes a numeric matrix, bootstrap resamples each row, and then
calculates the mean. The intended use case is for Bayesian posterior
predictions from sample data. Instead of directly calculating the
average marginal effect (AME) across all observed values, these can be
bootstrapped, so that uncertainty in the target population, and thus the
AME in the target population, can be incorporated. Model uncertainty is
already assumed to be handled by the different posterior samples, which
are assumed to be across rows.

## Usage

``` r
rowBootMeans(x)
```

## Arguments

- x:

  A numeric matrix

## Value

A numeric vector with the simple bootstrapped row means of the matrix

## Examples

``` r
x <- matrix(1:9, byrow = TRUE, 3)
replicate(10, rowBootMeans(x))
#>          [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
#> [1,] 2.333333 1.666667 2.666667 2.666667 2.333333 2.000000 2.333333 2.000000
#> [2,] 4.666667 4.666667 4.666667 4.666667 6.000000 4.333333 4.666667 5.000000
#> [3,] 7.666667 7.666667 8.666667 8.000000 8.000000 7.666667 7.666667 7.666667
#>          [,9]    [,10]
#> [1,] 1.666667 2.333333
#> [2,] 6.000000 4.333333
#> [3,] 7.666667 8.333333
```
