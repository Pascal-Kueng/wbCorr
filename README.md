# wbCorr: Bivariate Within- and Between-Cluster Correlations

[![CRAN status](https://www.r-pkg.org/badges/version/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![R-CMD-check](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml)

The wbCorr package computes bivariate within- and between-cluster correlations for clustered data, such as repeated measures nested in persons, dyads, teams, or other groups. Results can be inspected as tables, matrices, and plots.

## Installation

Install the released version from CRAN:

```r
install.packages("wbCorr")
```

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("Pascal-Kueng/wbCorr")
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

print(correlations)

tables <- get_table(correlations)
matrices <- summary(correlations)

tables$within
tables$between
matrices$merged_wb

plot(correlations, "within")
plot(correlations, "between")
```

## Usage

1. Create an object using `wbCorr(data, cluster = "cluster_column")`. Printing the object shows the head of the tables.
2. Retrieve full tables with `get_table()` or `get_tables()`.
3. Retrieve correlation matrices with `summary()`, `get_matrix()`, or `get_matrices()`.
4. Plot within- or between-cluster correlations with `plot()`.

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
```

#### Sample output from `get_table()`

Calling `get_table()` on a `wbCorr` object returns two tables: one for within-cluster correlations and one for between-cluster correlations. The abridged display below omits the trailing count and status diagnostics described under Implementation details. See `?get_table` for more information and arguments.

```text
# Sample output
> get_table(wbCorrObject)
$within
  Parameter1 Parameter2    r       95% CI t(1598)         p
1       Var1       Var2 0.08 [0.03, 0.13]    3.25   0.001**
2       Var1       Var3 0.25 [0.21, 0.30]   10.44 < .001***
3       Var2       Var3 0.79 [0.77, 0.80]   50.89 < .001***

$between
  Parameter1 Parameter2     r         95% CI t(78)         p
1       Var1       Var2 -0.59 [-0.72, -0.43] -6.48 < .001***
2       Var1       Var3 -0.38 [-0.56, -0.18] -3.65 < .001***
3       Var2       Var3 -0.03  [-0.25, 0.19] -0.24     0.814
```

#### Sample output from `summary()` or `get_matrix()`

Calling `summary()` or `get_matrix()` on a `wbCorr` object returns correlation matrices. The merged matrices show within- and between-cluster correlations above and below the diagonal.

```text
> summary(wbCorrObject)
$within
        Var1    Var2    Var3
Var1    1.00  0.08** 0.25***
Var2  0.08**    1.00 0.79***
Var3 0.25*** 0.79***    1.00

$between
         Var1     Var2     Var3
Var1     1.00 -0.59*** -0.38***
Var2 -0.59***     1.00    -0.03
Var3 -0.38***    -0.03     1.00

$merged_wb
         Var1   Var2    Var3
Var1     1.00 0.08** 0.25***
Var2 -0.59***   1.00 0.79***
Var3 -0.38***  -0.03    1.00

$merged_bw
        Var1     Var2     Var3
Var1    1.00 -0.59*** -0.38***
Var2  0.08**     1.00    -0.03
Var3 0.25***  0.79***     1.00

```

## Implementation details

For every variable pair, wbCorr computes the correlation on rows where both variables and the cluster variable are observed. This means missing data are handled pairwise for the bivariate association.

The detailed within and between tables always keep one row per requested pair, including pairs that cannot be estimated. `n_obs` counts jointly observed raw rows with a nonmissing cluster ID, while `n_clusters` counts clusters contributing at least one such row. `status`/`reason` describe coefficient estimability; `inference_status`/`inference_reason` separately distinguish complete, partial, unavailable, and unrequested inference. A coefficient needs two varying analysis units, while p-values or intervals can require more. Correlation-matrix diagonals are 1 only when the variable has positive variance at that level; otherwise they and all p-value diagonals are `NA`.

When the cluster identifier is a column in the analysis data, prefer `cluster = "column_name"`. If a separate vector is supplied, wbCorr excludes any data column containing the same identifiers and missing-value pattern, allowing common integer, numeric, factor, or character representations; a named column avoids ambiguity with a genuine outcome that happens to contain the same values.

Logical inputs are encoded as 0/1. Factors must declare exactly two levels and are encoded as 0/1 in their declared level order; character inputs must first be converted to factors so that this orientation is explicit. Other categorical factors are not accepted: use meaningful numeric scores for ordered variables or dummy-code nominal variables. Numeric `Inf`, `-Inf`, and `NaN` values are treated as missing before estimation.

The within-cluster correlation is the pooled residual correlation: each observed value is centered around its cluster mean for that same variable pair, and the correlation is computed on those residuals. For Pearson within-cluster correlations, analytic tests use `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of complete observation pairs and `k_pair` is the number of clusters contributing at least one complete pair. Pearson p-values use the corresponding t test, while confidence intervals use Fisher's z transformation and are always bounded by -1 and 1. These analytic results are working approximations for clustered data because residual pairs may still be dependent within clusters.

The between-cluster correlation is computed from cluster means. By default, `between_weighting = "equal_clusters"` gives every cluster the same weight. Use `between_weighting = "cluster_size"` to compute a sample-size weighted correlation of cluster means, where the weight is the number of complete observation pairs in each cluster. The ordinary Pearson t test and Fisher-z interval do not apply to this weighted coefficient, so wbCorr omits analytic p-values and confidence intervals for it. Use `inference = "cluster_bootstrap"` when weighted inference is required.

With `method = "spearman"`, wbCorr reports Spearman's correlation of mean-centered scores within clusters and Spearman's correlation of cluster means between clusters. These are descriptive mean-based decompositions, not the transformation-invariant clustered rank parameters of Tu, Li, and Shepherd (2025). Analytic and row-wise jackknife inference are therefore not provided. Use `inference = "cluster_bootstrap"` for a whole-cluster bootstrap interval. Cluster-size-weighted Spearman is not supported because wbCorr does not currently define a weighted-rank estimand.

The ICC shown for each variable is the one-way random-effects, single-measure ICC(1,1). wbCorr estimates it from all finite observations for that variable with the ANOVA method of moments, including the unequal-cluster-size adjustment. A sample ICC can be negative when the between-cluster mean square is smaller than the within-cluster mean square; wbCorr retains that information instead of truncating it to zero. Under severe imbalance the raw ANOVA estimate can be less than -1. Its population interpretation assumes independent clusters, a common within-cluster variance, and noninformative cluster size and missingness. The ICC is `NA` when there are too few clusters, no within-cluster replication, or no variability.

For resampling intervals that preserve top-level dependence, use `inference = "cluster_bootstrap"`. This resamples whole top-level clusters, recomputes the selected decomposition in each bootstrap sample, and reports first-order percentile intervals. It does not report bootstrap p-values. Their accuracy assumes independent clusters and adequate numbers of clusters and bootstrap replicates. `n_boot_attempted` and `n_boot_valid` expose the Monte Carlo yield; inference is marked partial if invalid replicate coefficients are excluded and unavailable if fewer than 10 valid coefficients remain. The technical minimum is useful only for quick tests—use substantially more and assess Monte Carlo stability for substantive analyses. Use `inference = "none"` to report coefficients without p-values or confidence intervals.

By default, `centering_rows = "pairwise_complete"` estimates cluster means from the same complete-pair row set used for the correlation. This keeps the within residuals centered for the actual pairwise sample and makes the between correlation a correlation of matched pair-specific cluster means.

Alternatively, `centering_rows = "all_available"` estimates each variable's cluster mean from all available rows for that variable. This can make each univariate cluster mean more stable when data are missing, and it mirrors a common multilevel-model preprocessing workflow where person means are created before the model applies complete-case filtering. That workflow is fine and defensible in multilevel models. In wbCorr, however, the variables are treated symmetrically as a descriptive bivariate decomposition, so all-available centering means the two cluster means in a pair may be based on different occasions. For that reason, pairwise-complete centering remains the default, and analytic inference with all-available centering is marked as approximate.

> Note. This decomposition supports binary indicators because their cluster means are interpretable proportions. Multi-level categorical variables require meaningful numeric scoring or dummy coding before use.

## Citation

Please cite the CRAN release as:

> Küng, P. (2026). *wbCorr: Bivariate Within- and Between-Cluster Correlations* (R package version 0.3.1). [https://doi.org/10.32614/CRAN.package.wbCorr](https://doi.org/10.32614/CRAN.package.wbCorr)

Generate the citation and BibTeX entry in R with:

```r
citation("wbCorr")
```
