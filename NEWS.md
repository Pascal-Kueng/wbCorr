# wbCorr 0.4.0

## Strictly nested three-level correlations

* Added a backward-compatible three-level decomposition for observations
  nested in lower-level units nested in upper-level units. Supply a fully
  named list ordered lower-to-higher, for example
  `cluster = list(person = "person_id", dyad = "dyad_id")`.
* The three manifest Pearson estimands are now explicit: observation
  deviations from pair-specific lower-unit means (`level1`), lower-unit means
  centered around equal-lower-unit upper means (`level2`), and those
  equal-lower-unit upper means (`level3`). Observations, lower units, and upper
  units contribute equally at their respective levels. With unequal numbers of
  observations per lower unit, this equal-lower-unit upper mean deliberately
  differs from the conventional mean of all observations in an upper unit.
* Added a dedicated `wbCorrNested` result with separate tables, matrices,
  positive-semidefinite diagnostics, and plots for all three levels. Merged
  triangle matrices and a single ICC are deliberately unavailable because
  neither has an unambiguous three-level meaning.
* Added whole-upper-cluster percentile bootstrap intervals that preserve all
  descendant observations, retain pairwise missingness in the sampling frame,
  and re-key duplicated hierarchy paths. Analytic three-level inference remains
  unavailable.
* Added strict validation for globally unique nesting, complete hierarchy
  paths, supported v1 options, pairwise/listwise missing-data handling, and
  pair-specific `n_obs`, `n_level2`, and `n_level3` diagnostics.
* Preserved the existing scalar column-name and external-vector API, numerical
  results, accessors, and object layout for ordinary two-level analyses.

# wbCorr 0.3.2

## Statistical correctness

* Reimplemented the one-way random-effects, single-measure ICC(1,1) with the
  ANOVA method-of-moments formula for unequal cluster sizes. Negative sample
  ICCs are retained, and non-estimable cases now return explicit diagnostics.
* Corrected Pearson analytic degrees of freedom and Fisher-z confidence
  intervals. Intervals are bounded by -1 and 1, and inference that is not
  supported by the available analysis units is reported as unavailable.
* Clarified that the Spearman option is a descriptive correlation of
  mean-centered scores within clusters and of cluster means between clusters.
  Unsupported analytic, row-jackknife, and cluster-size-weighted Spearman
  inference has been removed; whole-cluster bootstrap intervals remain
  available.

## Inputs, diagnostics, and output

* Strengthened validation for data, cluster identifiers, method, confidence
  levels, bootstrap settings, weighting, and centering choices.
* Logical variables and declared two-level factors are supported with explicit
  0/1 orientation. Other categorical inputs are rejected with guidance, and
  non-finite numeric values are treated as missing.
* Detailed tables now retain every requested variable pair, including pairs
  whose coefficient or inference cannot be estimated. Pair-specific observation
  counts, cluster counts, coefficient status, inference status, reasons, and
  bootstrap yield diagnostics are reported.
* Correlation-matrix diagonals are 1 only for variables with positive variance
  at the relevant level; otherwise they are `NA`.
* Added positive-semidefinite diagnostics for the unrounded within- and
  between-cluster matrices. Pairwise matrices that are not positive
  semidefinite now trigger a targeted warning, and `missing_data = "listwise"`
  provides an optional common-row analysis.
* `get_matrix(..., numeric = TRUE)` now returns unrounded numeric within,
  between, and merged matrices, including unrounded ICC diagonals.
* `print()` now provides concise, executable guidance for inspecting summaries
  and extracting full tables, numeric matrices, and ICCs.
* Plot panels now use the exact pair-specific rows, centering rule, method, and
  between-cluster weights used by the fitted object. Annotations come from the
  stored coefficient and p-value, and significance legends are omitted when
  p-values were not requested or are unavailable.
* `to_excel()` now accepts a data frame or matrix directly, supports mixed
  lists of tabular objects, errors clearly when there is nothing to write, and
  invisibly returns the supplied output path.

## Development and release preparation

* Added reference tests against `psych::statsBy()` for within- and
  between-cluster correlations and ICC(1,1), and expanded automated
  regression-test coverage.
* Added a pkgdown website workflow and a vignette introducing the distinction
  between within- and between-cluster correlations.
* Removed obsolete self-generated test fixtures, a generated plot file, and
  tracked R session history. Added safeguards against including regenerated
  session or graphics artifacts in the repository and source package.
* Corrected the bundled-data documentation to report 5,000 observations from
  100 participants measured over 50 days.
* Added repository citation metadata and a DOI badge that use the permanent
  Zenodo concept DOI, `10.5281/zenodo.20357592`, rather than a release-specific
  DOI.
