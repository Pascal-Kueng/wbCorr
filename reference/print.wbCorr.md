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
                       method = 'spearman')
print(correlations)
#> 
#> ---- wbCorr Object ----
#> Call:  wbCorr(data = simdat_intensive_longitudinal, cluster = "participantID",      confidence_level = 0.95, method = "spearman") 
#> 
#>  Within-Cluster Correlations: 
#> ---------------------------- 
#>   Parameter1 Parameter2 spearman's rho z-statistic   df        95% CI         p
#> 1        day       var1          -0.01       -0.36 4898 [-0.03, 0.02]     0.718
#> 2        day       var2          -0.01       -0.78 4898 [-0.04, 0.02]     0.433
#> 3        day       var3           0.12        8.39 4898  [0.09, 0.15] < .001***
#> 4       var1       var2           0.12        8.39 4898  [0.09, 0.15] < .001***
#> 5       var1       var3           0.78       72.42 4898  [0.76, 0.79] < .001***
#> 6       var2       var3           0.29       21.00 4898  [0.27, 0.32] < .001***
#> 
#>  Between-Cluster Correlations: 
#> ----------------------------- 
#>   Parameter1 Parameter2 spearman's rho z-statistic df         95% CI         p
#> 1        day       var1             NA          NA 97       [NA, NA]      <NA>
#> 2        day       var2             NA          NA 97       [NA, NA]      <NA>
#> 3        day       var3             NA          NA 97       [NA, NA]      <NA>
#> 4       var1       var2          -0.55       -6.12 97 [-0.68, -0.40] < .001***
#> 5       var1       var3          -0.22       -2.16 97 [-0.40, -0.02]    0.031*
#> 6       var2       var3          -0.05       -0.48 97  [-0.24, 0.15]     0.631
#> 
#>  Intraclass Correlation Coefficients: 
#> ------------------------------------ 
#>   variable         ICC
#> 1      day -0.02040816
#> 2     var1  0.51348244
#> 3     var2  0.49354674
#> 4     var3  0.49258895
#> 
#> Access full tables with get_tables(object, which = c('within', 'between'))
#> Access correlation matrices with summary(object, which = c('within', 'between', 'merge'))
#> Access full ICC list with get_ICC(object)
```
