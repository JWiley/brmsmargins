# Check a [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html) Object has Random Effects

Internal utility function to check whether a
[brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
object has any random effects or not.

## Usage

``` r
is.random(object)
```

## Arguments

- object:

  An object to be evaluated.

## Value

`TRUE` if any random effects present. `FALSE` if no random effects
present.
