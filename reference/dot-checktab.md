# Check Object Class is a Table

Internal utility function confirm that an object has the attributes
needed to be used as data. Currently it should be a `tbl`, `data.frame`,
or `data.table`.

## Usage

``` r
.checktab(x, requireNames = TRUE)
```

## Arguments

- x:

  An object to be evaluated.

- requireNames:

  A logical, whether names are required. Defaults to `TRUE`

## Value

An empty string if no issues. Otherwise, a non zero string with
warning/error messages.
