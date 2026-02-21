# Convert a Link Function Name to a List

Internal utility function used in
[`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md).
Takes a link function name as a character string, the type of effect to
be used, and the desired back transformation and returns a list with all
the options needed to execute the desired options in
[`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md).

## Usage

``` r
.links(
  link,
  effects = c("fixedonly", "includeRE", "integrateoutRE"),
  backtrans = c("response", "linear", "identity", "invlogit", "exp", "square", "inverse")
)
```

## Arguments

- link:

  The link named in a
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
  object

- effects:

  A character string, the type of effect desired

- backtrans:

  A character string, the type of back transformation

## Value

A list with eight elements.

- scale A character string giving the argument to be passed to
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html).

- ilink A character string giving the name of the inverse link function.

- ifun Inverse link function as an `R` function.

- ilinknum An integer giving the inverse link / transformation to be
  applied in
  [`integratere()`](https://joshuawiley.com/brmsmargins/reference/integratere.md),
  needed as this is a C++ function and cannot use the `R` based inverse
  link function.
