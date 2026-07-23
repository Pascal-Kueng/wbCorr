# wbCorr 0.3.2 (development version)

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
* Plotting now respects the selected between-cluster weighting and avoids
  misleading significance legends when p-values were not requested or are not
  available.

## Development and release preparation

* Added reference tests against `psych::ICC()` and expanded automated R CMD
  check coverage.
* Added repository citation metadata and a DOI badge that use the permanent
  Zenodo concept DOI, `10.5281/zenodo.20357592`, rather than a release-specific
  DOI.
