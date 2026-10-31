# wbCorr: Bivariate Within- and Between-Cluster Correlations

[![CRAN status](https://www.r-pkg.org/badges/version/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/wbCorr)](https://CRAN.R-project.org/package=wbCorr)
[![R-CMD-check](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Pascal-Kueng/wbCorr/actions/workflows/R-CMD-check.yaml)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20357592-blue.svg)](https://doi.org/10.5281/zenodo.20357592)

The wbCorr package computes bivariate within- and between-cluster correlations for clustered data, such as repeated measures nested in persons, dyads, teams, or other groups. Results can be inspected as tables, matrices, and plots.

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
```

### Extract results

`get_table()` returns the detailed pairwise results. Use `summary()` for
formatted matrices.

```r
tables <- get_table(correlations)
tables$within
tables$between

summary(correlations, "wb")
```

## Implementation details

See `?wbCorr` for how the correlations and their tests are computed.

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
