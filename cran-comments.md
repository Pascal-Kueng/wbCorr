## Resubmission

This is a resubmission. In this version I have:

* Removed the redundant phrase "The wbCorr package" from the Description field.
* Added methodological references to the Description field:
  Tu et al. (2025) <doi:10.1002/sim.10326>,
  Curran and Bauer (2011) <doi:10.1146/annurev.psych.093008.100356>, and
  Hamaker (2024) <doi:10.1080/00273171.2022.2155930>.
* Added missing \value documentation for exported plot, print, and show
  methods.
* Replaced \dontrun{} with \donttest{} in the wbCorr examples.
* Removed internal set.seed() calls from jackknife and bootstrap helper
  functions.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission

The NOTE is expected because this package is not yet on CRAN.
