# Return all ICCs for the original variables.

You can use get_ICC() or get_ICCs() interchangeably.

## Usage

``` r
get_ICC(object)

get_ICCs(object)

get_icc(object)
```

## Arguments

- object:

  A wbCorr object, created by the wbCorr() function.

## Value

A data frame with the one-way random-effects, single-measure ICC(1,1)
for every variable. Each ICC is estimated separately from all finite
observations with a non-missing cluster identifier. The ANOVA
method-of-moments estimator uses an effective cluster size for
unbalanced clusters. Negative sample estimates are retained and can be
less than -1 in severely unbalanced samples. The population
interpretation assumes independent clusters, a common within-cluster
variance, and noninformative cluster size and missingness. `NA` is
returned when an ICC cannot be estimated because there are fewer than
two clusters, no within-cluster replication, or zero total variability.

## References

Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
assessing rater reliability. *Psychological Bulletin, 86*(2), 420-428.
[doi:10.1037/0033-2909.86.2.420](https://doi.org/10.1037/0033-2909.86.2.420)

Ohyama, T. (2025). A comparison of confidence interval methods for the
intraclass correlation coefficient based on the one-way random effects
model. *Japanese Journal of Statistics and Data Science, 8*, 587-602.
[doi:10.1007/s42081-025-00292-3](https://doi.org/10.1007/s42081-025-00292-3)

Wang, C.-M., Yandell, B. S., & Rutledge, J. J. (1992). The dilemma of
negative analysis of variance estimators of intraclass correlation.
*Theoretical and Applied Genetics, 85*, 79-88.
[doi:10.1007/BF00223848](https://doi.org/10.1007/BF00223848)

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)

## Examples

``` r
# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')
#> Warning: Analytic p-values and confidence intervals are working approximations for clustered data; use inference = 'cluster_bootstrap' for whole-cluster resampling intervals.

# returns the ICCs:
ICCs <- get_ICC(correlations)
print(ICCs)
#>   variable         ICC
#> 1      day -0.02040816
#> 2     var1  0.51348244
#> 3     var2  0.49354674
#> 4     var3  0.49258895
```
