# Calculate Marginal Effects from 'brms' Models

This function is designed to help calculate marginal effects including
average marginal effects (AMEs) from `brms` models. Arguments are
labeled as *required* when it is required that the user directly specify
the argument. Arguments are labeled as *optional* when either the
argument is optional or there are sensible default values so that users
do not typically need to specify the argument.

## Usage

``` r
brmsmargins(
  object,
  at = NULL,
  wat = NULL,
  add = NULL,
  newdata = model.frame(object),
  CI = 0.99,
  CIType = "HDI",
  contrasts = NULL,
  ROPE = NULL,
  MID = NULL,
  subset = NULL,
  dpar = NULL,
  seed,
  verbose = FALSE,
  ...
)
```

## Arguments

- object:

  A *required* argument specifying a fitted `brms` model object.

- at:

  An *optional* argument (but note, either `at` or `add` are *required*)
  specifying an object inheriting from data frame indicating the values
  to hold specific variables at when calculating average predictions.
  This is intended for AMEs from categorical variables.

- wat:

  An *optional* list with named elements including one element named,
  "ID" with a single character string, the name of the variable in the
  model frame that is the ID variable. Additionally, there should be one
  or more named elements, named after variables in the model (and
  specified in the `at` argument), that contain a `data.table` or
  `data.frame` with three variables: (1) the ID variable giving IDs, (2)
  the values specified for the variable in the `at` argument, and (3)
  the actual values to be substituted for each ID. `wat` cannot be non
  null unless `at` also is non null.

- add:

  An *optional* argument (but note, either `at` or `add` are *required*)
  specifying an object inheriting from data frame indicating the values
  to add to specific variables at when calculating average predictions.
  This is intended for AMEs for continuous variables.

- newdata:

  An *optional* argument specifying an object inheriting from data frame
  indicating the baseline values to use for predictions and AMEs. It
  uses a sensible default: the model frame from the `brms` model object
  passed on the `object` argument.

- CI:

  An *optional* argument with a numeric value specifying the width of
  the credible interval. Defaults to `0.99`. This default is arbitrary,
  but is purposefully higher than the common `0.95` to encourage science
  with greater acknowledgment of uncertainty or larger sample sizes
  (ideally).

- CIType:

  An *optional* argument, a character string specifying the type of
  credible interval (e.g., highest density interval). It is passed down
  to
  [`bsummary()`](https://joshuawiley.com/brmsmargins/reference/bsummary.md)
  which in turn passes it to
  [`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html).
  Defaults to "HDI".

- contrasts:

  An *optional* argument specifying a contrast matrix. The posterior
  predictions matrix is post multiplied by the contrast matrix, so they
  must be conformable. The posterior predictions matrix has a separate
  column for each row in the `at` or `add` object, so the contrast
  matrix should have the same number of rows. It can have multiple
  columns, if you desire multiple specific contrasts.

- ROPE:

  An *optional* argument, that can either be left as `NULL`, the
  default, or a numeric vector of length 2, specifying the lower and
  upper thresholds for the Region of Practical Equivalence (ROPE).

- MID:

  An *optional* argument, that can either left as `NULL`, the default,
  or a numeric vector of length 2, specifying the lower and upper
  thresholds for a Minimally Important Difference (MID). Unlike the
  ROPE, percentages for the MID are calculated as at or exceeding the
  bounds specified by this argument, whereas the ROPE is the percentage
  of the posterior at or inside the bounds specified.

- subset:

  An *optional* argument, a character string that is a valid `R`
  expression used to subset the dataset passed in `newdata`, prior to
  analysis. Defaults to `NULL`.

- dpar:

  An *optional* argument giving the parameter passed on to the `dpar`
  argument of [`fitted()`](https://rdrr.io/r/stats/fitted.values.html)
  in brms. Defaults to `NULL`, indicating the mean or location parameter
  typically.

- seed:

  An *optional* argument that controls whether (and if so what) random
  seed to use. This does not matter when using fixed effects only.
  However, when using Monte Carlo integration to integrate out random
  effects from mixed effects models, it is critical if you are looking
  at a continuous marginal effect with some small offset value as
  otherwise the Monte Carlo error from one set of predictions to another
  may exceed the true predicted difference. If `seed` is left missing,
  the default, than a single, random integer between +\\ 1e7 is chosen
  and used to set the seed before each prediction. If manually chosen
  (recommended for reproducibility), the seed should either be a single
  value, in which case this single value is used to set the seed before
  each prediction. Alternately, it can be a vector of seeds with either
  the same length as the number of rows in `at` or `add`, whichever was
  specified. This is probably generally not what you want, as it means
  that even for the same input data, you would get slightly different
  predictions (when integrating out random effects) due to Monte Carlo
  variation. Finally, rather than being missing, you can explicitly set
  `seed = NULL`, if you do not want any seed to be set. This would be
  fine, for instance, when only using fixed effects, or if you know what
  you are doing and intend that behavior when integrating out random
  effects.

- verbose:

  An *optional* argument, a logical value whether to print more verbose
  messages. Defaults to `FALSE` which is quieter. Set to `TRUE` for more
  messages to be printed where relevant.

- ...:

  An *optional* argument, additional arguments passed on to
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md).
  In particular, the `effects` argument of
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md)
  is important for mixed effects models to control how random effects
  are treated in the predictions, which subsequently changes the
  marginal effect estimates.

## Value

A list with four elements.

- `Posterior` Posterior distribution of all predictions. These
  predictions default to fixed effects only, but by specifying options
  to
  [`prediction()`](https://joshuawiley.com/brmsmargins/reference/prediction.md)
  they can include random effects or be predictions integrating out
  random effects.

- `Summary` A summary of the predictions.

- `Contrasts` Posterior distribution of all contrasts, if a contrast
  matrix was specified.

- `ContrastSummary` A summary of the posterior distribution of all
  contrasts, if specified

## Details

The main parts required for the function are a fitted model object, (via
the `object` argument) a dataset to be used for prediction, (via the
`newdata` argument which defaults to the model frame), and a dataset
passed to either `at` or `add`. The steps are as follows:

1.  Check that the function inputs (model object, data, etc.) are valid.

2.  Take the dataset from the `newdata` argument and either add the
    values from the first row of `add` or replace the values using the
    first row of `at`. Only variables specified in `at` or `add` are
    modified. Other variables are left as is.

3.  Use the [`fitted()`](https://rdrr.io/r/stats/fitted.values.html)
    function to generate predictions based on this modified dataset. If
    `effects` is set to "fixedonly" (meaning only generate predictions
    using fixed effects) or to "includeRE" (meaning generate predictions
    using fixed and random effects), then predictions are generated
    entirely using the
    [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) function
    and are, typically back transformed to the response scale. For mixed
    effects models with fixed and random effects where `effects` is set
    to "integrateoutRE", then
    [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) is only
    used to generate predictions using the fixed effects on the linear
    scale. For each prediction generated, the random effects are
    integrated out by drawing `k` random samples from the model assumed
    random effect(s) distribution. These are added to the fixed effects
    predictions, back transformed, and then averaged over all `k` random
    samples to perform numerical Monte Carlo integration.

4.  All the predictions for each posterior draw, after any back
    transformation has been applied, are averaged, resulting in one,
    marginal value for each posterior draw. These are marginal
    predictions. They are average marginal predictions if averaging over
    the sample dataset, or may be marginal predictions at the means, if
    the initial input dataset used mean values, etc.

5.  Steps two to four are repeated for each row of `at` or `add`.
    Results are combined into a matrix where the columns are different
    rows from `at` or `add` and the rows are different posterior draws.

6.  If contrasts were specified, using a contrast matrix, the marginal
    prediction matrix is post multiplied by the contrast matrix.
    Depending on the choice(s) of `add` or `at` and the values in the
    contrast matrix, these can then be average marginal effects (AMEs)
    by using numerical integration (`add` with 0 and a very close to 0
    value) or discrete difference (`at` with say 0 and 1 as values) for
    a given predictor(s).

7.  The marginal predictions and the contrasts, if specified are
    summarized.

Although `brmsmargins()` is focused on helping to calculate marginal
effects, it can also be used to generate marginal predictions, and
indeed these marginal predictions are the foundation of any marginal
effect estimates. Through manipulating the input data, `at` or `add` and
the contrast matrix, other types of estimates averaged or weighting
results in specific ways are also possible.

## References

Pavlou, M., Ambler, G., Seaman, S., & Omar, R. Z. (2015)
[doi:10.1186/s12874-015-0046-6](https://doi.org/10.1186/s12874-015-0046-6)
“A note on obtaining correct marginal predictions from a random
intercepts model for binary outcomes” and Skrondal, A., & Rabe-Hesketh,
S. (2009)
[doi:10.1111/j.1467-985X.2009.00587.x](https://doi.org/10.1111/j.1467-985X.2009.00587.x)
“Prediction in multilevel generalized linear models” and Norton EC, Dowd
BE, Maciejewski ML. (2019)
[doi:10.1001/jama.2019.1954](https://doi.org/10.1001/jama.2019.1954)
“Marginal Effects—Quantifying the Effect of Changes in Risk Factors in
Logistic Regression Models”

## Examples

``` r
if (FALSE) { # \dontrun{
#### Testing ####
## sample data and logistic model with brms
set.seed(1234)
Tx <- rep(0:1, each = 50)
ybin <- c(rep(0:1, c(40,10)), rep(0:1, c(10,40)))
logitd <- data.frame(Tx = Tx, ybin = ybin)
logitd$x <- rnorm(100, mean = logitd$ybin, sd = 2)

mbin <- brms::brm(ybin ~ Tx + x, data = logitd, family = brms::bernoulli())

summary(mbin)

## now check AME for Tx
tmp <- brmsmargins(
  object = mbin,
  at = data.table::data.table(Tx = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp$Summary
tmp$ContrastSummary ## Tx AME


## now check AME for Tx with bootstrapping the AME population
tmpalt <- brmsmargins(
  object = mbin,
  at = data.table::data.table(Tx = 0:1),
  contrasts = matrix(c(-1, 1), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10),
  resample = 100L)

tmpalt$Summary
tmpalt$ContrastSummary ## Tx AME

## now check AME for continuous predictor, x
## use .01 as an approximation for first derivative
## 1 / .01 in the contrast matrix to get back to a one unit change metric
tmp2 <- brmsmargins(
  object = mbin,
  add = data.table::data.table(x = c(0, .01)),
  contrasts = matrix(c(-1/.01, 1/.01), nrow = 2),
  ROPE = c(-.05, +.05),
  MID = c(-.10, +.10))

tmp2$ContrastSummary ## x AME

if (FALSE) {
  library(lme4)
  data(sleepstudy)
  fit <- brms::brm(Reaction ~ 1 + Days + (1 + Days | Subject),
             data = sleepstudy,
             cores = 4)

  summary(fit, prob = 0.99)

  tmp <- brmsmargins(
    object = fit,
    at = data.table::data.table(Days = 0:1),
    contrasts = matrix(c(-1, 1), nrow = 2),
    ROPE = c(-.05, +.05),
    MID = c(-.10, +.10), CIType = "ETI", effects = "integrateoutRE", k = 5L)

  tmp$Summary
  tmp$ContrastSummary
  }
} # }
```
