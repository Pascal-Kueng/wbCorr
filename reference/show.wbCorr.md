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
cors <- wbCorr(iris, iris$Species, between_weighting = 'cluster_size')
show(cors)
#> 
#> ---- wbCorr Object ----
#> Call:  wbCorr(data = iris, cluster = iris$Species, between_weighting = "cluster_size") 
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
#> 
#>  Between-Cluster Correlations: 
#> ----------------------------- 
#>     Parameter1   Parameter2 pearson's r t-statistic df        95% CI      p
#> 1 Sepal.Length  Sepal.Width       -0.75       -1.12  1 [-1.00, 1.00]  0.465
#> 2 Sepal.Length Petal.Length        0.99        9.19  1 [-1.00, 1.00]  0.069
#> 3 Sepal.Length  Petal.Width        1.00       46.45  1 [-1.00, 1.00] 0.014*
#> 4  Sepal.Width Petal.Length       -0.81       -1.40  1 [-1.00, 1.00]  0.396
#> 5  Sepal.Width  Petal.Width       -0.76       -1.17  1 [-1.00, 1.00]  0.451
#> 6 Petal.Length  Petal.Width        1.00       11.49  1 [-1.00, 1.00]  0.055
#> 
#>  Intraclass Correlation Coefficients: 
#> ------------------------------------ 
#>       variable       ICC
#> 1 Sepal.Length 0.7028488
#> 2  Sepal.Width 0.4906278
#> 3 Petal.Length 0.9593219
#> 4  Petal.Width 0.9504463
#> 
#> Access full tables with get_tables(object, which = c('within', 'between'))
#> Access correlation matrices with summary(object, which = c('within', 'between', 'merge'))
#> Access full ICC list with get_ICC(object)
```
