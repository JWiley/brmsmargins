# Check Assertions about a [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html) class Model Object

These are a set of internal utility functions. They are not intended for
general use. Instead, they are intended to be called in circumstances
where the expected result is `TRUE`. All of them are designed to try to
give informative error messages if the assertion is not met. All of them
result in a `stop` error if the assertion is not met.

## Usage

``` r
.assertbrmsfit(object)

.assertRE(object)

.assertfamily(object)

.assertdpar(object, dpar)

.assertlink(object, dpar)
```

## Arguments

- object:

  A
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html)
  model object to be evaluated.

- dpar:

  Required for `.assertdpar()` which checks this is valid. Optional for
  `.assertlink()` which will use `NULL` if not specified. If specified,
  this should be `NULL` or a character string.

## Value

An invisible, logical `TRUE` if the assertion is met. An (informative)
error message if the assertion is not met.

## Details

- `.assertbrmsfit`: asserts that the object should be
  [brms::brmsfit](https://paulbuerkner.com/brms/reference/brmsfit-class.html).

- `.assertRE`: asserts that all random effects are Gaussian or
  student-t.

- `.assertfamily`: asserts that the distribution (family) of the outcome
  is a currently supported family. Only applies when integrating out
  random effects.

- `.assertlink`: asserts that the link function is a currently supported
  link function. Only applies when integrating out random effects.
