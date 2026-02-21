# Marginal Posterior Predictions from a 'brms' Model

Calculate marginal predictions from a `brms` model. Marginal predictions
average over the input data for each posterior draw. Marginal
predictions for models with random effects will integrate over random
effects. Arguments are labeled as *required* when it is required that
the user directly specify the argument. Arguments are labeled as
*optional* when either the argument is optional or there are sensible
default values so that users do not typically need to specify the
argument.

## Usage

``` r
prediction(
  object,
  data,
  summarize = TRUE,
  posterior = FALSE,
  index,
  dpar = NULL,
  resample = 0L,
  resampleseed = FALSE,
  effects = c("fixedonly", "includeRE", "integrateoutRE"),
  backtrans = c("response", "linear", "identity", "invlogit", "exp", "square", "inverse"),
  k = 100L,
  raw = FALSE,
  ...
)
```

## Arguments

- object:

  A *required* argument specifying a fitted `brms` model object.

- data:

  A *required* argument specifying a data frame or data table passed to
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) as the new
  data to be used for predictions.

- summarize:

  An *optional* argument, a logical value, whether or not to calculate
  summaries of the posterior predictions. Defaults to `TRUE`.

- posterior:

  An *optional* argument, a logical value whether or not to save and
  return the posterior samples. Defaults to `FALSE` as the assumption is
  a typical use case is to return the summaries only.

- index:

  An *optional* argument, an integer vector, giving the posterior draws
  to be used in the calculations. If omitted, defaults to all posterior
  draws.

- dpar:

  An *optional* argument, the parameter passed on to the `dpar` argument
  of [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) in brms.
  Defaults to `NULL` indicating the mean or location parameter
  typically.

- resample:

  An *optional* argument, an integer indicating the number of bootstrap
  resamples of the posterior predictions to use when calculating
  summaries. Defaults to `0L`. See documentation from
  [`.averagePosterior()`](https://joshuawiley.com/brmsmargins/reference/dot-averagePosterior.md)
  for more details. This should be considered experimental.

- resampleseed:

  An *optional* argument, a seed for random number generation. Defaults
  to `FALSE`, which means no seed is set. Only used if `resample` is a
  positive, non-zero integer. See documentation from
  [`.averagePosterior()`](https://joshuawiley.com/brmsmargins/reference/dot-averagePosterior.md)
  for more details. This should be considered experimental.

- effects:

  An *optional* argument, a character string indicating the type of
  prediction to be made. Can be one of "fixedonly" meaning only use
  fixed effects, "includeRE" meaning that random effects should be
  included in the predictions, or "integrateoutRE" meaning that random
  effects should be integrated out / over in the predictions. It
  defaults to "fixedonly" so is not typically required for a user to
  specify it.

- backtrans:

  An *optional* argument, a character string indicating the type of back
  transformation to be applied. Can be one of "response" meaning to use
  the response scale, "linear" or "identity" meaning to use the linear
  predictor scale, or a specific back transformation desired, from a
  possible list of "invlogit", "exp", "square", or "inverse". Custom
  back transformations should only be needed if, for example, the
  outcome variable was transformed prior to fitting the model. It
  defaults to "response" so is not typically required for a user to
  specify it.

- k:

  An *optional* argument, an integer providing the number of random
  draws to use for integrating out the random effects. Only relevant
  when `effects = "integrateoutRE"`. It defaults to `100L`, a rather
  arbitrary number attempting to balance the increased precision that
  comes from a larger value, with the increased computational cost of
  more Monte Carlo simulations when integrating out random effects.

- raw:

  An *optional* argument, a logical value indicating whether to return
  the raw output or to average over the Monte Carlo samples. Defaults to
  `FALSE`. Setting it to `TRUE` can be useful if you want not only the
  full posterior distribution but also the `k` Monte Carlo samples used
  for the numerical integration. This cannot be used with
  `summarize = TRUE`.

- ...:

  Additional arguments passed to
  [`bsummary()`](https://joshuawiley.com/brmsmargins/reference/bsummary.md),
  and only relevant if `summarize` is `TRUE`.

## Value

A list with `Summary` and `Posterior`. Some of these may be `NULL`
depending on the arguments used.

## References

Pavlou, M., Ambler, G., Seaman, S., & Omar, R. Z. (2015)
[doi:10.1186/s12874-015-0046-6](https://doi.org/10.1186/s12874-015-0046-6)
“A note on obtaining correct marginal predictions from a random
intercepts model for binary outcomes” and Skrondal, A., & Rabe-Hesketh,
S. (2009)
[doi:10.1111/j.1467-985X.2009.00587.x](https://doi.org/10.1111/j.1467-985X.2009.00587.x)
“Prediction in multilevel generalized linear models”
