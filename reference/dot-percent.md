# Calculate Percent of Observations Within or Without a Window

This is an internal helper function to calculate and label the
percentage of a posterior distribution that falls within the Region of
Practical Equivalence (ROPE) or at or beyond a Minimally Important
Difference (MID). It is designed to fail gracefully if no window given,
and to give some useful labels about the windows / range used. Intended
for use internally as part of
[`brmsmargins()`](https://joshuawiley.com/brmsmargins/reference/brmsmargins.md).

## Usage

``` r
.percent(x, window = NULL, within = TRUE)
```

## Arguments

- x:

  A vector of values to evaluate. Required.

- window:

  An optional numeric vector giving a window.

- within:

  A logical value indicating whether to calculate the percentage within
  the window (if `TRUE`) or the percentage at or outside the window (if
  `FALSE`). Defaults to `TRUE`.

## Value

A list with the `Window`, if specified else `NULL`, the `Percent` of
observations, and a `Label` specifying the exact window used in human
readable format.
