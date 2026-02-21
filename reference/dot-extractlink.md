# Extract the Link from a `brms` Model

Internal utility function to take a
[brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
object and extract the link for a specific brms `dpar`.

## Usage

``` r
.extractlink(object, dpar)
```

## Arguments

- object:

  A
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
  model object.

- dpar:

  The dpar for which the link should be extracted.

## Value

A character string, the link.
