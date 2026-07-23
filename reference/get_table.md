# Retrieve full tables for both within- and/or between-cluster correlations for a wbCorr object.

This function has an alias get_tables() which can be used
interchangeably. For correlations matrices, see the summary() function.

## Usage

``` r
get_table(object, which = c("within", "between"))

get_tables(object, which = c("within", "between"))
```

## Arguments

- object:

  A wbCorr object, created by the wbCorr() function.

- which:

  A character vector indicating which correlation table to return.
  Options are 'within' or 'w', and 'between' or 'b'.

## Value

A list containing the selected detailed tables of within- and/or
between-cluster correlations. Each table retains every requested pair
and includes raw pair-row, contributing-cluster, bootstrap-yield,
coefficient- status, and inference-status diagnostics; see
[`wbCorr()`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)
for definitions.

## See also

[`summary`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md),
[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)

## Examples

``` r
# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')
#> Warning: Analytic p-values and confidence intervals are working approximations for clustered data; use inference = 'cluster_bootstrap' for whole-cluster resampling intervals.

# returns a list with full detailed tables of the correlations:
tables <- get_table(correlations) # the get_tables() function is equivalent
print(tables)
#> $within
#>   Parameter1 Parameter2 pearson's r t-statistic   df        95% CI         p
#> 1        day       var1       -0.01       -0.64 4899 [-0.04, 0.02]     0.522
#> 2        day       var2       -0.01       -0.80 4899 [-0.04, 0.02]     0.424
#> 3        day       var3        0.12        8.67 4899  [0.10, 0.15] < .001***
#> 4       var1       var2        0.12        8.53 4899  [0.09, 0.15] < .001***
#> 5       var1       var3        0.79       89.04 4899  [0.78, 0.80] < .001***
#> 6       var2       var3        0.29       21.50 4899  [0.27, 0.32] < .001***
#>   n_obs n_clusters n_boot_attempted n_boot_valid status reason inference_status
#> 1  5000        100               NA           NA     ok   <NA>               ok
#> 2  5000        100               NA           NA     ok   <NA>               ok
#> 3  5000        100               NA           NA     ok   <NA>               ok
#> 4  5000        100               NA           NA     ok   <NA>               ok
#> 5  5000        100               NA           NA     ok   <NA>               ok
#> 6  5000        100               NA           NA     ok   <NA>               ok
#>   inference_reason
#> 1             <NA>
#> 2             <NA>
#> 3             <NA>
#> 4             <NA>
#> 5             <NA>
#> 6             <NA>
#> 
#> $between
#>   Parameter1 Parameter2 pearson's r t-statistic df         95% CI         p
#> 1        day       var1          NA          NA NA           <NA>      <NA>
#> 2        day       var2          NA          NA NA           <NA>      <NA>
#> 3        day       var3          NA          NA NA           <NA>      <NA>
#> 4       var1       var2       -0.60       -7.46 98 [-0.71, -0.46] < .001***
#> 5       var1       var3       -0.24       -2.43 98 [-0.42, -0.04]    0.017*
#> 6       var2       var3       -0.04       -0.44 98  [-0.24, 0.15]     0.659
#>   n_obs n_clusters n_boot_attempted n_boot_valid        status
#> 1  5000        100               NA           NA not_estimable
#> 2  5000        100               NA           NA not_estimable
#> 3  5000        100               NA           NA not_estimable
#> 4  5000        100               NA           NA            ok
#> 5  5000        100               NA           NA            ok
#> 6  5000        100               NA           NA            ok
#>                     reason inference_status          inference_reason
#> 1 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 2 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 3 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 4                     <NA>               ok                      <NA>
#> 5                     <NA>               ok                      <NA>
#> 6                     <NA>               ok                      <NA>
#> 

# Access specific tables by:
# Option 1:
tables$between
#>   Parameter1 Parameter2 pearson's r t-statistic df         95% CI         p
#> 1        day       var1          NA          NA NA           <NA>      <NA>
#> 2        day       var2          NA          NA NA           <NA>      <NA>
#> 3        day       var3          NA          NA NA           <NA>      <NA>
#> 4       var1       var2       -0.60       -7.46 98 [-0.71, -0.46] < .001***
#> 5       var1       var3       -0.24       -2.43 98 [-0.42, -0.04]    0.017*
#> 6       var2       var3       -0.04       -0.44 98  [-0.24, 0.15]     0.659
#>   n_obs n_clusters n_boot_attempted n_boot_valid        status
#> 1  5000        100               NA           NA not_estimable
#> 2  5000        100               NA           NA not_estimable
#> 3  5000        100               NA           NA not_estimable
#> 4  5000        100               NA           NA            ok
#> 5  5000        100               NA           NA            ok
#> 6  5000        100               NA           NA            ok
#>                     reason inference_status          inference_reason
#> 1 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 2 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 3 zero_variance_parameter1      unavailable coefficient_not_estimable
#> 4                     <NA>               ok                      <NA>
#> 5                     <NA>               ok                      <NA>
#> 6                     <NA>               ok                      <NA>
# Option 2:
within_table <- get_tables(correlations, which = 'w') # or use 'within' or 'between'
print(within_table) # within_table could be saved to an excel or csv file (e.g., write.csv)
#> $within
#>   Parameter1 Parameter2 pearson's r t-statistic   df        95% CI         p
#> 1        day       var1       -0.01       -0.64 4899 [-0.04, 0.02]     0.522
#> 2        day       var2       -0.01       -0.80 4899 [-0.04, 0.02]     0.424
#> 3        day       var3        0.12        8.67 4899  [0.10, 0.15] < .001***
#> 4       var1       var2        0.12        8.53 4899  [0.09, 0.15] < .001***
#> 5       var1       var3        0.79       89.04 4899  [0.78, 0.80] < .001***
#> 6       var2       var3        0.29       21.50 4899  [0.27, 0.32] < .001***
#>   n_obs n_clusters n_boot_attempted n_boot_valid status reason inference_status
#> 1  5000        100               NA           NA     ok   <NA>               ok
#> 2  5000        100               NA           NA     ok   <NA>               ok
#> 3  5000        100               NA           NA     ok   <NA>               ok
#> 4  5000        100               NA           NA     ok   <NA>               ok
#> 5  5000        100               NA           NA     ok   <NA>               ok
#> 6  5000        100               NA           NA     ok   <NA>               ok
#>   inference_reason
#> 1             <NA>
#> 2             <NA>
#> 3             <NA>
#> 4             <NA>
#> 5             <NA>
#> 6             <NA>
#> 
```
