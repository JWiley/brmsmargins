# brmsmargins devel

* Fixed a bug when using `prediction()` with option `effects = "integrateoutRE"`
  when smooth terms were present. As `prediction()` underpins other functions, 
  such as `brmsmargins()` this issue also impacts those other functions.
* New function: `marginalcoef()` which calculates population averaged (marginal) coefficients
  for the fixed effects coefficients from mixed effects models using a method 
  described by Donald Hedeker, who joins the author team.
* Added more unit testing and vignettes.

# brmsmargins 0.1.1

* Fixed a bug preventing predictions integrating out random effects for mixed effects models with a random intercept only (reported in Issue#1). Thanks to @ajnafa for reporting.
* Added support for Gamma and Beta regression models.
* More extensive testing added.

# brmsmargins 0.1.0

* Initial release
