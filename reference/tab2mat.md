# Convert a Row of a Table to a Square Matrix

Utility function to convert a row matrix to a square matrix. Used as the
`brms` package returns things like the Cholesky decomposition matrix as
separate columns where rows are posterior draws. Not intended to be
called directly by most users.

## Usage

``` r
tab2mat(X)

tab2matR(X)
```

## Arguments

- X:

  a matrix

## Value

A numeric matrix with one row.

## Functions

- `tab2matR()`: Pure `R` implementation of `tab2mat()`.

## Examples

``` r
tab2mat(matrix(1:4, 1))
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    3    4
tab2mat(matrix(1:9, 1))
#>      [,1] [,2] [,3]
#> [1,]    1    2    3
#> [2,]    4    5    6
#> [3,]    7    8    9
```
