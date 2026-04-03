# Personal Preference Based Bayesian Summary

Returns a summary of a posterior distribution for a single parameter /
value. It is based on personal preference. Notably, it does not only use
[`bayestestR::describe_posterior()`](https://rdrr.io/pkg/bayestestR/man/describe_posterior.html),
an excellent function, because of the desire to also describe the
percentage of the full posterior distribution that is at or exceeding
the value of a Minimally Important Difference (MID). MIDs are used in
clinical studies with outcome measures where there are pre-defined
differences that are considered clinically important, which is distinct
from the ROPE or general credible intervals capturing uncertainty.

## Usage

``` r
bsummary(x, CI = 0.99, CIType = "HDI", ROPE = NULL, MID = NULL)
```

## Arguments

- x:

  The posterior distribution of a parameter

- CI:

  A numeric value indicating the desired width of the credible interval.
  Defaults to `0.99` currently, but this is subject to change. a 99%
  interval was chosen as the default as there have been recent arguments
  made in the realm of meta science that there are, essentially, too
  many false positives and that many of the "findings" in science are
  not able to be replicated. In any case, users should ideally specify a
  desired CI width, and not rely on defaults.

- CIType:

  A character string indicating the type of credible interval, passed on
  to the
  [`bayestestR::ci()`](https://rdrr.io/pkg/bayestestR/man/ci.html)
  function as the method for CIs.

- ROPE:

  Either left as `NULL`, the default, or a numeric vector of length 2,
  specifying the lower and upper thresholds for the Region of Practical
  Equivalence (ROPE).

- MID:

  Either left as `NULL`, the default, or a numeric vector of length 2,
  specifying the lower and upper thresholds for a Minimally Important
  Difference (MID). Unlike the ROPE, percentages for the MID are
  calculated as at or exceeding the bounds specified by this argument,
  whereas the ROPE is the percentage of the posterior at or inside the
  bounds specified.

## Value

A `data.table` with:

- `M` the mean of the posterior samples

- `Mdn` the median of the posterior samples

- `LL` the lower limit of the credible interval

- `UL` the upper limit of the credible interval

- `PercentROPE` the percentage of posterior samples falling into the
  ROPE

- `PercentMID` the percentage of posterior samples falling at or beyond
  the MID

- `CI` the width of the credible interval used

- `CIType` the type of credible interval used (e.g., highest density
  interval)

- `ROPE` a label describing the values included in the ROPE

- `MID` a label describing the values included in the MID

## References

Kruschke, J. K. (2018).
[doi:10.1177/2515245918771304](https://doi.org/10.1177/2515245918771304)
“Rejecting or accepting parameter values in Bayesian estimation”

## Examples

``` r
bsummary(rnorm(1000))
#>              M         Mdn        LL       UL PercentROPE PercentMID    CI
#>          <num>       <num>     <num>    <num>       <num>      <num> <num>
#> 1: 0.004339615 0.008579051 -2.645212 2.411659          NA         NA  0.99
#>    CIType   ROPE    MID
#>    <char> <char> <char>
#> 1:    HDI   <NA>   <NA>

bsummary(rnorm(1000), ROPE = c(-.5, .5), MID = c(-1, 1))
#>              M         Mdn        LL       UL PercentROPE PercentMID    CI
#>          <num>       <num>     <num>    <num>       <num>      <num> <num>
#> 1: -0.01626336 -0.05466379 -2.422911 2.368512        38.7       30.9  0.99
#>    CIType        ROPE                   MID
#>    <char>      <char>                <char>
#> 1:    HDI [-0.5, 0.5] [-Inf, -1] | [1, Inf]
```
