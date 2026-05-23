# wbCorr: Bivariate Within- and Between-Cluster Correlations

The wbCorr package computes bivariate within- and between-cluster correlations for clustered data, such as repeated measures nested in persons, dyads, teams, or other groups. Results can be inspected as tables, matrices, and plots.

> Check installation steps and usage examples below the technical implementation

## Implementation Details

For every variable pair, wbCorr computes the correlation on rows where both variables and the cluster variable are observed. This means missing data are handled pairwise for the bivariate association.

The within-cluster correlation is the pooled residual correlation: each observed value is centered around its cluster mean for that same variable pair, and the correlation is computed on those residuals. For Pearson within-cluster correlations, analytic tests use `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of complete observation pairs and `k_pair` is the number of clusters contributing at least one complete pair. These analytic tests are working approximations for clustered data because residual pairs may still be dependent within clusters.

The between-cluster correlation is computed from cluster means. By default, `between_weighting = "equal_clusters"` gives every cluster the same weight. Use `between_weighting = "cluster_size"` to compute a sample-size weighted correlation of cluster means, where the weight is the number of complete observation pairs in each cluster.

For publication-level inference in intensive longitudinal data, prefer `inference = "cluster_bootstrap"`. This resamples whole top-level clusters, recomputes the selected decomposition in each bootstrap sample, and reports percentile bootstrap confidence intervals. Use `inference = "none"` to report coefficients without p-values or confidence intervals.

By default, `centering_rows = "pairwise_complete"` estimates cluster means from the same complete-pair row set used for the correlation. This keeps the within residuals centered for the actual pairwise sample and makes the between correlation a correlation of matched pair-specific cluster means.

Alternatively, `centering_rows = "all_available"` estimates each variable's cluster mean from all available rows for that variable. This can make each univariate cluster mean more stable when data are missing, and it mirrors a common multilevel-model preprocessing workflow where person means are created before the model applies complete-case filtering. That workflow is fine and defensible in multilevel models. In wbCorr, however, the variables are treated symmetrically as a descriptive bivariate decomposition, so all-available centering means the two cluster means in a pair may be based on different occasions. For that reason, pairwise-complete centering remains the default, and analytic inference with all-available centering is marked as approximate.

> Note. In most cases this is not appropriate for categorical data. Only use data than can be meaningfully centered around it's mean (e.g., interval and ratio data). 

## Installation
You can install this package by running the following inside an R-terminal:

``` R
install.packages('remotes')
remotes::install_github('Pascal-Kueng/wbCorr')
```

## Usage
1. Create an object using `wbCorr(data, cluster = "cluster_column")`. Printing the object shows the head of the tables.
2. Retrieve full tables with `get_table()` or `get_tables()`.
3. Retrieve correlation matrices with `summary()`, `get_matrix()`, or `get_matrices()`.
4. Plot within- or between-cluster correlations with `plot()`.

## Quick start
```R
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

### Check documentation
```R
?wbCorr # view documentation
```

### Common choices
```R
# Default coefficients with analytic, approximate p-values and CIs.
wbCorr(simdat_intensive_longitudinal, cluster = "participantID")

# Recommended for publication-level inference in EMA/daily diary data:
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

#### Sample Output for get_table()
using function `get_table()` on a 'wbCorr' object will provide you with two tables, one for within- and one for between- cluster correlations.
see ?get_table() for more information and arguments. 
``` 
# Sample output
> get_table(wbCorrObject)
$within
  Parameter1 Parameter2    r       95% CI t(1598)         p
1       Var1       Var2 0.08 [0.03, 0.13]    3.25   0.001**
2       Var1       Var3 0.25 [0.21, 0.30]   10.44 < .001***
3       Var2       Var3 0.79 [0.76, 0.82]   50.89 < .001***

$between
  Parameter1 Parameter2     r         95% CI t(78)         p
1       Var1       Var2 -0.59 [-0.77, -0.41] -6.48 < .001***
2       Var1       Var3 -0.38 [-0.59, -0.17] -3.65 < .001***
3       Var2       Var3 -0.03  [-0.25, 0.20] -0.24     0.814
```

#### Sample Output for summary() or get_matrix()
using `summary()` or `get_matrix` on a 'wbCorr' object will create matrices. The merged matrices provide within- and between- correlations with one above, and one below the diagonal. 
```
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

## Citation
Please cite this package using:
```
citation('wbCorr')
```
