# wbCorr: Bivariate Within- and Between-Cluster Correlations

[![CRAN status](https://www.r-pkg.org/badges/version/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![R-CMD-check](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20357592-blue.svg)](https://doi.org/10.5281/zenodo.20357592)

The wbCorr package computes bivariate level-specific correlations for clustered
data, including observations nested in one cluster level and strictly nested
three-level structures. Results can be inspected as tables, matrices, and
plots.

Developed by [Pascal Küng](https://pascalkueng.com/), Postdoctoral Researcher
at the [University of Zurich](https://www.psychology.uzh.ch/en/areas/sob/angsoz/team/kueng.html).
[OSF profile](https://osf.io/r2qdg/).

[Read the introductory vignette: within- and between-cluster correlations](https://pascal-kueng.github.io/wbCorr/articles/within-between-correlations.html)

## Installation

Install the released version from CRAN:

```r
install.packages("wbCorr")
```

Install the development version from GitHub:

```r
install.packages('wbCorr', repos = c('https://pascal-kueng.r-universe.dev', 'https://cloud.r-project.org'))
```

## Quick start

```r
library(wbCorr)

data("simdat_intensive_longitudinal")

correlations <- wbCorr(
  data = simdat_intensive_longitudinal,
  cluster = "participantID",
  inference = "cluster_bootstrap",
  nboot = 1000
)

summary(correlations, "w")  # within-cluster correlation matrix
summary(correlations, "b")  # between-cluster correlation matrix
summary(correlations, "wb") # within above, between below, ICC on the diagonal

plot(correlations, "within")
plot(correlations, "between")
```

## Usage

1. Create an object using `wbCorr(data, cluster = "cluster_column")`.
2. Inspect formatted matrices with `summary(object, "w")`, `summary(object, "b")`, or `summary(object, "wb")`.
3. Retrieve full tables with `get_table()` or `get_tables()`.
4. Retrieve unrounded matrices for downstream calculations with `get_matrix(object, numeric = TRUE)`.
5. Plot within- or between-cluster correlations with `plot()`.

For a strict three-level hierarchy, use a named list whose names label the
grouping levels and whose values name data columns. Order it lower-to-higher:

```r
nested_correlations <- wbCorr(
  data = my_data,
  cluster = list(person = "person_id", dyad = "dyad_id"),
  inference = "none"
)

summary(nested_correlations, "l1") # observations within people
summary(nested_correlations, "l2") # people within dyads
summary(nested_correlations, "l3") # dyads

get_table(nested_correlations)
plot(nested_correlations, "l2")
```

Three-level output is kept in three separate matrices. A merged triangle and a
single ICC are not defined for this object.

### Check documentation

```r
?wbCorr # view documentation
```

### Common choices

```r
# Default coefficients with analytic, approximate p-values and CIs.
wbCorr(simdat_intensive_longitudinal, cluster = "participantID")

# Whole-cluster resampling intervals for EMA/daily diary data:
# resample participants and recompute all correlations.
wbCorr(simdat_intensive_longitudinal,
       cluster = "participantID",
       inference = "cluster_bootstrap",
       nboot = 1000)

# Coefficients only, without p-values or CIs.
wbCorr(simdat_intensive_longitudinal,
       cluster = "participantID",
       inference = "none")

# Between-cluster correlations weighted by the number of complete pairs in
# each cluster.
wbCorr(simdat_intensive_longitudinal,
       cluster = "participantID",
       between_weighting = "cluster_size")

# Estimate cluster means from all rows available for each variable, similar to
# common multilevel-model preprocessing.
wbCorr(simdat_intensive_longitudinal,
       cluster = "participantID",
       centering_rows = "all_available")

# Use one common complete-row sample when a positive-semidefinite correlation
# matrix is required (at the cost of discarding partially observed rows).
wbCorr(simdat_intensive_longitudinal,
       cluster = "participantID",
       missing_data = "listwise")
```

### Extract results

`get_table()` returns the detailed pairwise results and diagnostics. Use
`summary()` for formatted display matrices and `get_matrix(..., numeric = TRUE)`
for unrounded coefficients in downstream calculations.

```r
tables <- get_table(correlations)
tables$within
tables$between

summary(correlations, "wb")

numeric_matrices <- get_matrix(
  correlations,
  which = c("w", "b", "wb"),
  numeric = TRUE
)
numeric_matrices$within
numeric_matrices$merged_wb
```

## Implementation details

By default, wbCorr computes each correlation on rows where that variable pair and the cluster identifier are observed. Different pairs can therefore use different samples, which can make a completed pairwise correlation matrix non-positive-semidefinite. wbCorr checks the unrounded within- and between-cluster matrices, warns when either is not positive semidefinite, and reports the minimum-eigenvalue diagnostics through `get_matrix_diagnostics()`.

Use `missing_data = "listwise"` to retain one row set that is complete across every supported analysis variable and the cluster identifier before the within/between decomposition and bootstrap. This produces a positive-semidefinite complete matrix when entries use one common coefficient method and weight vector, but it can discard substantially more data. Matrices containing unavailable coefficients are reported as not assessable. Merged matrices are not checked because their triangles represent different levels and their diagonal contains ICCs; they are display hybrids, not correlation matrices.

The detailed within and between tables always keep one row per requested pair, including pairs that cannot be estimated. `n_obs` counts jointly observed raw rows with a nonmissing cluster ID, while `n_clusters` counts clusters contributing at least one such row. `status`/`reason` describe coefficient estimability; `inference_status`/`inference_reason` separately distinguish complete, partial, unavailable, and unrequested inference. A coefficient needs two varying analysis units, while p-values or intervals can require more. Correlation-matrix diagonals are 1 only when the variable has positive variance at that level; otherwise they and all p-value diagonals are `NA`.

When the cluster identifier is a column in the analysis data, prefer `cluster = "column_name"`. If a separate vector is supplied, wbCorr excludes any data column containing the same identifiers and missing-value pattern, allowing common integer, numeric, factor, or character representations; a named column avoids ambiguity with a genuine outcome that happens to contain the same values.

For three-level data, `cluster = list(lower = "lower_id", upper = "upper_id")`
selects a sequential manifest decomposition. For each variable pair, level 1
correlates observations after subtracting pair-specific lower-unit means; level
2 correlates lower-unit means after subtracting their upper-unit mean; and
level 3 correlates those upper-unit means. Observations, lower units, and upper
units contribute equally at their respective levels. Lower IDs must be
globally unique and map to exactly one upper ID. The initial implementation
supports Pearson coefficients with `inference = "none"` or whole-upper-cluster
bootstrap intervals, pairwise-complete centering, equal-unit weighting, and
pairwise or listwise missing-data handling.

When lower units contribute different numbers of observations, this
equal-lower-unit upper mean is not the conventional mean of all observations
in the upper unit. The two definitions coincide when lower-unit observation
counts are balanced. This is a package-defined descriptive target, not a
latent multilevel parameter.

Logical inputs are encoded as 0/1. Factors must declare exactly two levels and are encoded as 0/1 in their declared level order; character inputs must first be converted to factors so that this orientation is explicit. Other categorical factors are not accepted: use meaningful numeric scores for ordered variables or dummy-code nominal variables. Numeric `Inf`, `-Inf`, and `NaN` values are treated as missing before estimation.

The within-cluster correlation is the pooled residual correlation: each observed value is centered around its cluster mean for that same variable pair, and the correlation is computed on those residuals. For Pearson within-cluster correlations, analytic tests use `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of complete observation pairs and `k_pair` is the number of clusters contributing at least one complete pair. Pearson p-values use the corresponding t test, while confidence intervals use Fisher's z transformation and are always bounded by -1 and 1. These analytic results are working approximations for clustered data because residual pairs may still be dependent within clusters.

The between-cluster correlation is computed from cluster means. By default, `between_weighting = "equal_clusters"` gives every cluster the same weight. Use `between_weighting = "cluster_size"` to compute a sample-size weighted correlation of cluster means, where the weight is the number of complete observation pairs in each cluster. The ordinary Pearson t test and Fisher-z interval do not apply to this weighted coefficient, so wbCorr omits analytic p-values and confidence intervals for it. Use `inference = "cluster_bootstrap"` when weighted inference is required.

With `method = "spearman"`, wbCorr reports Spearman's correlation of mean-centered scores within clusters and Spearman's correlation of cluster means between clusters. These are descriptive mean-based decompositions, not the transformation-invariant clustered rank parameters of Tu, Li, and Shepherd (2025). Analytic and row-wise jackknife inference are therefore not provided. Use `inference = "cluster_bootstrap"` for a whole-cluster bootstrap interval. Cluster-size-weighted Spearman is not supported because wbCorr does not currently define a weighted-rank estimand.

The ICC shown for each variable is the one-way random-effects, single-measure ICC(1,1). wbCorr estimates it from all finite observations for that variable with the ANOVA method of moments, including the unequal-cluster-size adjustment. A sample ICC can be negative when the between-cluster mean square is smaller than the within-cluster mean square; wbCorr retains that information instead of truncating it to zero. Under severe imbalance the raw ANOVA estimate can be less than -1. Its population interpretation assumes independent clusters, a common within-cluster variance, and noninformative cluster size and missingness. The ICC is `NA` when there are too few clusters, no within-cluster replication, or no variability.

For resampling intervals that preserve top-level dependence, use `inference = "cluster_bootstrap"`. This resamples whole top-level clusters, recomputes the selected decomposition in each bootstrap sample, and reports first-order percentile intervals. It does not report bootstrap p-values. Their accuracy assumes independent clusters and adequate numbers of clusters and bootstrap replicates. `n_boot_attempted` and `n_boot_valid` expose the Monte Carlo yield; inference is marked partial if invalid replicate coefficients are excluded and unavailable if fewer than 10 valid coefficients remain. The technical minimum is useful only for quick tests—use substantially more and assess Monte Carlo stability for substantive analyses. Use `inference = "none"` to report coefficients without p-values or confidence intervals.

Plots reconstruct every panel from the same pair-specific rows, centering rule, method, and between-cluster weights used for the stored coefficient. Panel annotations use the stored coefficient and p-value rather than a separately fitted surrogate statistic.

By default, `centering_rows = "pairwise_complete"` estimates cluster means from the same complete-pair row set used for the correlation. This keeps the within residuals centered for the actual pairwise sample and makes the between correlation a correlation of matched pair-specific cluster means.

Alternatively, `centering_rows = "all_available"` estimates each variable's cluster mean from all available rows for that variable. This can make each univariate cluster mean more stable when data are missing, and it mirrors a common multilevel-model preprocessing workflow where person means are created before the model applies complete-case filtering. That workflow is fine and defensible in multilevel models. In wbCorr, however, the variables are treated symmetrically as a descriptive bivariate decomposition, so all-available centering means the two cluster means in a pair may be based on different occasions. For that reason, pairwise-complete centering remains the default, and analytic inference with all-available centering is marked as approximate.

> Note. This decomposition supports binary indicators because their cluster means are interpretable proportions. Multi-level categorical variables require meaningful numeric scoring or dummy coding before use.

## Citation

Please cite wbCorr as:

> Küng, P. (2026). *wbCorr: Bivariate Within- and Between-Cluster Correlations* (R package version 0.3.2). [https://doi.org/10.5281/zenodo.20357592](https://doi.org/10.5281/zenodo.20357592)

This is the version-independent Zenodo concept DOI for the repository. It
resolves to the latest archived release; release-specific Zenodo DOIs are
intentionally not used in repository metadata.

Generate the citation and BibTeX entry in R with:

```r
citation("wbCorr")
```
