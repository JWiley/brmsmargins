# Marginal Coefficients from a 'brms' Model

Calculate marginal coefficients from a `brms` generalized linear mixed
model using the method proposed by Hedeker (2018).

## Usage

``` r
marginalcoef(
  object,
  summarize = TRUE,
  posterior = FALSE,
  index,
  backtrans = c("response", "linear", "identity", "invlogit", "exp", "square", "inverse"),
  k = 100L,
  seed,
  ...
)
```

## Arguments

- object:

  A fitted `brms` model object that includes random effects. Required.

- summarize:

  A logical value, whether or not to calculate summaries of the
  posterior predictions. Defaults to `TRUE`.

- posterior:

  A logical value whether or not to save and return the posterior
  samples. Defaults to `FALSE` as the assumption is a typical use case
  is to return the summaries only.

- index:

  An optional integer vector, giving the posterior draws to be used in
  the calculations. If omitted, defaults to all posterior draws.

- backtrans:

  A character string indicating the type of back transformation to be
  applied. Can be one of "response" meaning to use the response scale,
  "linear" or "identity" meaning to use the linear predictor scale, or a
  specific back transformation desired, from a possible list of
  "invlogit", "exp", "square", or "inverse". Custom back transformations
  should only be needed if, for example, the outcome variable was
  transformed prior to fitting the model.

- k:

  An integer providing the number of random draws to use for integrating
  out the random effects. Only relevant when
  `effects = "integrateoutRE"`.

- seed:

  An *optional* argument that controls whether (and if so what) random
  seed to use. This can help with reproducibility of results. It is
  missing by default.

- ...:

  Additional arguments passed to
  [`bsummary()`](https://joshuawiley.com/brmsmargins/reference/bsummary.md),
  and only relevant if `summarize` is `TRUE`.

## Value

A list with `Summary` and `Posterior`. Some of these may be `NULL`
depending on the arguments used.

## References

Hedeker, D., du Toit, S. H., Demirtas, H. & Gibbons, R. D. (2018)
[doi:10.1111/biom.12707](https://doi.org/10.1111/biom.12707) . “A note
on marginalization of regression parameters from mixed models of binary
outcomes”
