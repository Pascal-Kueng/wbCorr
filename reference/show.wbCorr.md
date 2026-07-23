# Show Method for the wbCorr Class

Shows a summary of the `wbCorr` object, equivalent to the print method.

## Usage

``` r
# S4 method for class 'wbCorr'
show(object)
```

## Arguments

- object:

  A `wbCorr` object.

## Value

Invisibly returns the supplied `wbCorr` object. Called for the side
effect of showing the same compact summary as
[`print()`](https://rdrr.io/r/base/print.html).

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md),
[`print.wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/print.wbCorr.md)

## Examples

``` r
# Example using the iris dataset
cors <- wbCorr(iris, iris$Species, weighted_between_statistics = TRUE)
#> Warning: Analytic p-values and confidence intervals are working approximations for clustered data; use inference = 'cluster_bootstrap' for whole-cluster resampling intervals.
#> Warning: Analytic inference is not supported for cluster-size-weighted between correlations; returning the weighted coefficient without a p-value or confidence interval. Use inference = 'cluster_bootstrap' for weighted inference.
show(cors)
#> 
#> ---- wbCorr Object ----
#> Call:  wbCorr(data = iris, cluster = iris$Species, weighted_between_statistics = TRUE) 
#> 
#>  Within-Cluster Correlations: 
#> ---------------------------- 
#>     Parameter1   Parameter2 pearson's r t-statistic  df       95% CI         p
#> 1 Sepal.Length  Sepal.Width        0.53        7.56 146 [0.40, 0.64] < .001***
#> 2 Sepal.Length Petal.Length        0.76       13.96 146 [0.68, 0.82] < .001***
#> 3 Sepal.Length  Petal.Width        0.36        4.73 146 [0.22, 0.50] < .001***
#> 4  Sepal.Width Petal.Length        0.38        4.93 146 [0.23, 0.51] < .001***
#> 5  Sepal.Width  Petal.Width        0.47        6.44 146 [0.33, 0.59] < .001***
#> 6 Petal.Length  Petal.Width        0.48        6.69 146 [0.35, 0.60] < .001***
#>   n_obs n_clusters n_boot_attempted n_boot_valid status reason inference_status
#> 1   150          3               NA           NA     ok   <NA>               ok
#> 2   150          3               NA           NA     ok   <NA>               ok
#> 3   150          3               NA           NA     ok   <NA>               ok
#> 4   150          3               NA           NA     ok   <NA>               ok
#> 5   150          3               NA           NA     ok   <NA>               ok
#> 6   150          3               NA           NA     ok   <NA>               ok
#>   inference_reason
#> 1             <NA>
#> 2             <NA>
#> 3             <NA>
#> 4             <NA>
#> 5             <NA>
#> 6             <NA>
#> 
#>  Between-Cluster Correlations: 
#> ----------------------------- 
#>     Parameter1   Parameter2
#> 1 Sepal.Length  Sepal.Width
#> 2 Sepal.Length Petal.Length
#> 3 Sepal.Length  Petal.Width
#> 4  Sepal.Width Petal.Length
#> 5  Sepal.Width  Petal.Width
#> 6 Petal.Length  Petal.Width
#>                                                             warning pearson's r
#> 1 weighted between analytic inference unavailable; coefficient only       -0.75
#> 2 weighted between analytic inference unavailable; coefficient only        0.99
#> 3 weighted between analytic inference unavailable; coefficient only        1.00
#> 4 weighted between analytic inference unavailable; coefficient only       -0.81
#> 5 weighted between analytic inference unavailable; coefficient only       -0.76
#> 6 weighted between analytic inference unavailable; coefficient only        1.00
#>   t-statistic df 95% CI  p n_obs n_clusters n_boot_attempted n_boot_valid
#> 1          NA NA   <NA> NA   150          3               NA           NA
#> 2          NA NA   <NA> NA   150          3               NA           NA
#> 3          NA NA   <NA> NA   150          3               NA           NA
#> 4          NA NA   <NA> NA   150          3               NA           NA
#> 5          NA NA   <NA> NA   150          3               NA           NA
#> 6          NA NA   <NA> NA   150          3               NA           NA
#>   status reason inference_status                        inference_reason
#> 1     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 2     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 3     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 4     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 5     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 6     ok   <NA>      unavailable weighted_analytic_inference_unsupported
#> 
#>  Intraclass Correlation Coefficients: 
#> ------------------------------------ 
#>       variable       ICC
#> 1 Sepal.Length 0.7028488
#> 2  Sepal.Width 0.4906278
#> 3 Petal.Length 0.9593219
#> 4  Petal.Width 0.9504463
#> 
#> Inspect matrices with summary(object, which = c('w', 'b', 'wb'))
#> Access full tables with get_tables(object, which = c('within', 'between'))
#> Access matrices programmatically with get_matrix(object, numeric = TRUE)
#> Access the full ICC table with get_ICC(object)
```
