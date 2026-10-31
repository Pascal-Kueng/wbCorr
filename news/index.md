# Changelog

## wbCorr (development version)

This version returns to the simpler design of earlier wbCorr versions,
while keeping the statistical fixes of 0.3.2.

### Breaking changes

- Cluster means are again estimated from all available observations of
  each variable, as in
  [`psych::statsBy()`](https://rdrr.io/pkg/psych/man/statsBy.html). The
  `centering_rows` argument is removed. Results only change when data
  are missing.
- Removed the arguments `between_inference` (use `inference = "none"`),
  `missing_data` (use
  [`na.omit()`](https://rdrr.io/r/stats/na.fail.html) for listwise
  deletion), and the deprecated `bootstrap` and
  `weighted_between_statistics`.
- Removed `get_matrix_diagnostics()` and `get_matrix(numeric = TRUE)`.
  Unrounded coefficients are stored in the wbCorr object (e.g.,
  `object@within$correlations`).
- Detailed tables no longer contain the diagnostic columns `n_obs`,
  `n_clusters`, `n_boot_attempted`, `n_boot_valid`, `status`, `reason`,
  `inference_status`, and `inference_reason`.

### Restored

- Spearman correlations have p-values and confidence intervals again
  (Fisher z).
- `method = "auto"` uses Spearman for ordered factors again.
- Cluster-size weighted between-cluster correlations have analytic
  p-values and confidence intervals again.
- The cluster bootstrap reports p-values again.

### Fixes

- Correlations based on two clusters are `NA` instead of a perfect
  correlation.
- [`wbCorr()`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)
  no longer warns on every call that analytic p-values are
  approximations.
- The cluster bootstrap reports bias-corrected (BC) intervals, works
  with cluster-size weighting, and is much faster.

### Documentation

- Rewrote the README with a worked example and its output. Removed the
  vignette, which repeated the README.

## wbCorr 0.3.2

CRAN release: 2026-07-23

### Statistical correctness

- Reimplemented the one-way random-effects, single-measure ICC(1,1) with
  the ANOVA method-of-moments formula for unequal cluster sizes.
  Negative sample ICCs are retained, and non-estimable cases now return
  explicit diagnostics.
- Corrected Pearson analytic degrees of freedom and Fisher-z confidence
  intervals. Intervals are bounded by -1 and 1, and inference that is
  not supported by the available analysis units is reported as
  unavailable.
- Clarified that the Spearman option is a descriptive correlation of
  mean-centered scores within clusters and of cluster means between
  clusters. Unsupported analytic, row-jackknife, and
  cluster-size-weighted Spearman inference has been removed;
  whole-cluster bootstrap intervals remain available.

### Inputs, diagnostics, and output

- Strengthened validation for data, cluster identifiers, method,
  confidence levels, bootstrap settings, weighting, and centering
  choices.
- Logical variables and declared two-level factors are supported with
  explicit 0/1 orientation. Other categorical inputs are rejected with
  guidance, and non-finite numeric values are treated as missing.
- Detailed tables now retain every requested variable pair, including
  pairs whose coefficient or inference cannot be estimated.
  Pair-specific observation counts, cluster counts, coefficient status,
  inference status, reasons, and bootstrap yield diagnostics are
  reported.
- Correlation-matrix diagonals are 1 only for variables with positive
  variance at the relevant level; otherwise they are `NA`.
- Added positive-semidefinite diagnostics for the unrounded within- and
  between-cluster matrices. Pairwise matrices that are not positive
  semidefinite now trigger a targeted warning, and
  `missing_data = "listwise"` provides an optional common-row analysis.
- `get_matrix(..., numeric = TRUE)` now returns unrounded numeric
  within, between, and merged matrices, including unrounded ICC
  diagonals.
- [`print()`](https://rdrr.io/r/base/print.html) now provides concise,
  executable guidance for inspecting summaries and extracting full
  tables, numeric matrices, and ICCs.
- Plot panels now use the exact pair-specific rows, centering rule,
  method, and between-cluster weights used by the fitted object.
  Annotations come from the stored coefficient and p-value, and
  significance legends are omitted when p-values were not requested or
  are unavailable.
- [`to_excel()`](https://pascal-kueng.github.io/wbCorr/reference/to_excel.md)
  now accepts a data frame or matrix directly, supports mixed lists of
  tabular objects, errors clearly when there is nothing to write, and
  invisibly returns the supplied output path.

### Development and release preparation

- Added reference tests against
  [`psych::statsBy()`](https://rdrr.io/pkg/psych/man/statsBy.html) for
  within- and between-cluster correlations and ICC(1,1), and expanded
  automated regression-test coverage.
- Added a pkgdown website workflow and a vignette introducing the
  distinction between within- and between-cluster correlations.
- Removed obsolete self-generated test fixtures, a generated plot file,
  and tracked R session history. Added safeguards against including
  regenerated session or graphics artifacts in the repository and source
  package.
- Corrected the bundled-data documentation to report 5,000 observations
  from 100 participants measured over 50 days.
- Added repository citation metadata and a DOI badge that use the
  permanent Zenodo concept DOI, `10.5281/zenodo.20357592`, rather than a
  release-specific DOI.
