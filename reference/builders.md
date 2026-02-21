# Build the Variable Names or Data Objects for Estimation

These are a set of internal utility functions. They are not intended for
general use.

## Usage

``` r
.namesL(block, number)

.buildL(data, block, number, dpar)

.namesSD(ranef, block, dpar)

.buildSD(data, ranef, block, dpar)

.namesZ(block, number, dpar)

.buildZ(data, block, number, dpar)
```

## Arguments

- block:

  Which random effect block to use. An integer.

- number:

  The number of elements in that random effect block. An integer.

- data:

  A data object. For example the result of
  [`brms::make_standata()`](https://paulbuerkner.com/brms/reference/standata.html)
  for `.buildZ()`, which is a list, or a dataset of the posterior draws
  such as from
  [`brms::as_draws_df()`](https://paulbuerkner.com/brms/reference/draws-brms.html)
  for `.buildL()` and `.buildSD()`.

- dpar:

  Which dpar to use. Does not apply to the L matrix.

- ranef:

  A data set with information about the model object random effects.
  Only used for `.namesSD()` and `.buildSD()`.

## Value

A character vector for all `.names` functions or a matrix for all
`.build` functions.

## Details

- `.namesL`: Generate names of an L matrix from brms. Create the
  variable names for the Cholesky decomposition of the random effects
  correlation matrix in
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
  Note that
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  returns the lower triangular matrix and we want the upper triangular
  matrix, so the names are transposed. The results can then be passed to
  [`tab2mat()`](https://joshuawiley.com/brmsmargins/reference/tab2mat.md)
  to convert the row vector into a matrix.

- `.buildL`: Returns the L matrix object. Rows are posterior draws.

- `.namesSD`: Create the names of random effect standard deviation
  estimates.

- `.buildSD`: Return matrix of random effect standard deviation
  estimates. Rows are posterior draws.

- `.namesZ`: Create the names of random effects data for predictions.

- `.buildZ`: Return matrix of data for random effect predictions.
