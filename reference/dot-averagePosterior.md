# Average Over Posterior Predictions

Internal function that averages over posterior predictions using either
[`rowMeans()`](https://rdrr.io/r/base/colSums.html) or
[`rowBootMeans()`](https://joshuawiley.com/brmsmargins/reference/rowBootMeans.md),
the latter being useful to incorporate uncertainty from the inputs being
used to generate predictions.

## Usage

``` r
.averagePosterior(posterior, resample = 0L, seed = FALSE)
```

## Arguments

- posterior:

  A posterior matrix type object. It is assumed that different
  predictions to be averaged over are on different columns. Different
  posterior draws are on different rows.

- resample:

  An integer indicating the number of bootstrap resamples of the
  posterior predictions to use when calculating summaries. Defaults to
  `0L`. See the details section for more information as its
  implementation is experimental and it may not operate as one would
  expect.

- seed:

  A seed for random number generation. Defaults to `FALSE`, which means
  no seed is set. Only used if `resample` is a positive, non-zero
  integer.

## Value

A vector of the averaged posterior.
