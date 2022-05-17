# brmsmargins 0.2.0

* Fixed a bug when using `prediction()` with option `effects = "integrateoutRE"`
  when smooth terms were present. As `prediction()` underpins other functions, 
  such as `brmsmargins()` this issue also impacts those other functions.
* New function: `marginalcoef()` which calculates population averaged (marginal) coefficients
  for the fixed effects coefficients from mixed effects models using a method 
  described by Donald Hedeker, who joins the author team.
  Currently, only the main location parameter is supported. That is, 
  marginal coefficients for the scale part of a model, in location and scale models,
  is not currently supported.
* New argument, `wat`, added to `brmsmargins()` to support including
  calculating average marginal effects for multilevel centered 
  categorical predictors.
* Updates to vignettes demonstrating: (1) the use of marginal coefficients;
  (2) marginal effects for centered categorical predictors; and
  (3) 'simple' marginal effects when models include interaction terms.
* Revised documentation for `bmrsmargins()` and `prediction()` to be clearer 
  around which arguments users must directly specify and which are optional or 
  have sensible defaults.
* Added more unit testing and vignettes.

# brmsmargins 0.1.1

* Fixed a bug preventing predictions integrating out random effects for mixed effects models with a random intercept only (reported in Issue#1). Thanks to @ajnafa for reporting.
* Added support for Gamma and Beta regression models.
* More extensive testing added.

# brmsmargins 0.1.0

* Initial release
