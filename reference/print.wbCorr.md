# Print Method for the wbCorr Class

Prints a summary of the `wbCorr` object.

## Usage

``` r
# S4 method for class 'wbCorr'
print(x, ...)
```

## Arguments

- x:

  A `wbCorr` object.

- ...:

  Additional arguments, currently unused.

## Value

Invisibly returns the supplied `wbCorr` object. Called for the side
effect of printing a compact summary of the within-cluster table,
between-cluster table, and ICC table.

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)

## Examples

``` r
# Example
data("simdat_intensive_longitudinal")
correlations <- wbCorr(simdat_intensive_longitudinal,
                       cluster = 'participantID',
                       confidence_level = 0.95,
                       method = 'spearman',
                       weighted_between_statistics = FALSE)
#> Warning: Analytic inference is not supported for wbCorr's descriptive clustered Spearman coefficients; returning coefficients only. Use inference = 'cluster_bootstrap' for whole-cluster bootstrap intervals.
print(correlations)
#> 
#> ---- wbCorr Object ----
#> Call:  wbCorr(data = simdat_intensive_longitudinal, cluster = "participantID",      confidence_level = 0.95, method = "spearman", weighted_between_statistics = FALSE) 
#> 
#>  Within-Cluster Correlations: 
#> ---------------------------- 
#>   Parameter1 Parameter2 centered-score Spearman rho n_obs n_clusters
#> 1        day       var1                       -0.01  5000        100
#> 2        day       var2                       -0.01  5000        100
#> 3        day       var3                        0.12  5000        100
#> 4       var1       var2                        0.12  5000        100
#> 5       var1       var3                        0.78  5000        100
#> 6       var2       var3                        0.29  5000        100
#>   n_boot_attempted n_boot_valid status reason inference_status
#> 1               NA           NA     ok   <NA>      unavailable
#> 2               NA           NA     ok   <NA>      unavailable
#> 3               NA           NA     ok   <NA>      unavailable
#> 4               NA           NA     ok   <NA>      unavailable
#> 5               NA           NA     ok   <NA>      unavailable
#> 6               NA           NA     ok   <NA>      unavailable
#>                            inference_reason
#> 1 analytic_inference_unsupported_for_method
#> 2 analytic_inference_unsupported_for_method
#> 3 analytic_inference_unsupported_for_method
#> 4 analytic_inference_unsupported_for_method
#> 5 analytic_inference_unsupported_for_method
#> 6 analytic_inference_unsupported_for_method
#> 
#>  Between-Cluster Correlations: 
#> ----------------------------- 
#>   Parameter1 Parameter2 cluster-mean Spearman rho n_obs n_clusters
#> 1        day       var1                        NA  5000        100
#> 2        day       var2                        NA  5000        100
#> 3        day       var3                        NA  5000        100
#> 4       var1       var2                     -0.55  5000        100
#> 5       var1       var3                     -0.22  5000        100
#> 6       var2       var3                     -0.05  5000        100
#>   n_boot_attempted n_boot_valid        status                   reason
#> 1               NA           NA not_estimable zero_variance_parameter1
#> 2               NA           NA not_estimable zero_variance_parameter1
#> 3               NA           NA not_estimable zero_variance_parameter1
#> 4               NA           NA            ok                     <NA>
#> 5               NA           NA            ok                     <NA>
#> 6               NA           NA            ok                     <NA>
#>   inference_status                          inference_reason
#> 1      unavailable                 coefficient_not_estimable
#> 2      unavailable                 coefficient_not_estimable
#> 3      unavailable                 coefficient_not_estimable
#> 4      unavailable analytic_inference_unsupported_for_method
#> 5      unavailable analytic_inference_unsupported_for_method
#> 6      unavailable analytic_inference_unsupported_for_method
#> 
#>  Intraclass Correlation Coefficients: 
#> ------------------------------------ 
#>   variable         ICC
#> 1      day -0.02040816
#> 2     var1  0.51348244
#> 3     var2  0.49354674
#> 4     var3  0.49258895
#> 
#> Inspect matrices with summary(object, which = c('w', 'b', 'wb'))
#> Access full tables with get_tables(object, which = c('within', 'between'))
#> Access matrices programmatically with get_matrix(object, numeric = TRUE)
#> Access the full ICC table with get_ICC(object)
```
