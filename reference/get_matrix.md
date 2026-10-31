# Return matrices for within- and/or between-cluster correlations.

You can use summary(), get_matrices(), or get_matrix() interchangeably.
Merged matrices include the ICC on the diagonal. For more detailed
statistics, use get_table().

## Usage

``` r
get_matrix(object, which = c("within", "between", "merge"), ...)

get_matrices(object, which = c("within", "between", "merge"), ...)

# S3 method for class 'wbCorr'
summary(object, ...)

# S4 method for class 'wbCorr'
summary(object, which = c("within", "between", "merge"), ...)
```

## Arguments

- object:

  A wbCorr object, created by the wbCorr() function.

- which:

  A string or a character vector indicating which summaries to return.
  Options are 'within' or 'w', 'between' or 'b', and various merge
  options like 'merge', 'm', 'merge_wb', 'wb', 'merge_bw', 'bw'. Default
  is c('within', 'between', 'merge').

- ...:

  Additional arguments passed to the base summary method

## Value

A list containing the selected matrices of within- and/or
between-cluster correlations, and ICCs on the diagonals for merged
matrices.

## See also

[`get_tables`](https://pascal-kueng.github.io/wbCorr/reference/get_table.md),
[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)

## Examples

``` r
# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns a correlation matrix with stars for p-values:
matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
print(matrices)
#> $within
#>          day    var1    var2    var3
#> day        1   -0.01   -0.01 0.12***
#> var1   -0.01       1 0.12*** 0.79***
#> var2   -0.01 0.12***       1 0.29***
#> var3 0.12*** 0.79*** 0.29***       1
#> 
#> $between
#>      day     var1     var2   var3
#> day   NA       NA       NA     NA
#> var1  NA        1 -0.60*** -0.24*
#> var2  NA -0.60***        1  -0.04
#> var3  NA   -0.24*    -0.04      1
#> 
#> $merged_wb
#>          day     var1    var2    var3
#> day  [-0.02]    -0.01   -0.01 0.12***
#> var1      NA   [0.51] 0.12*** 0.79***
#> var2      NA -0.60***  [0.49] 0.29***
#> var3      NA   -0.24*   -0.04  [0.49]
#> 
#> $note_wb
#> [1] "Top-right triangle: Within-correlations. Bottom-left triangle: Between-correlations. Diagonal: ICC"
#> 
#> $merged_bw
#>          day    var1     var2   var3
#> day  [-0.02]      NA       NA     NA
#> var1   -0.01  [0.51] -0.60*** -0.24*
#> var2   -0.01 0.12***   [0.49]  -0.04
#> var3 0.12*** 0.79***  0.29*** [0.49]
#> 
#> $note_bw
#> [1] "Top-right triangle: Between-correlations. Bottom-left triangle: Within-correlations. Diagonal: ICC"
#> 
#> $note
#> [1] "***p < 0.001, **p < 0.01, *p < 0.05"
#> 

# Access specific matrices by:
# Option 1:
matrices$within
#>          day    var1    var2    var3
#> day        1   -0.01   -0.01 0.12***
#> var1   -0.01       1 0.12*** 0.79***
#> var2   -0.01 0.12***       1 0.29***
#> var3 0.12*** 0.79*** 0.29***       1
# Option 2:
within_matrix <- summary(correlations, which = 'w') # or use 'within'
merged_within_between <- summary(correlations, which = 'wb')
print(within_matrix) # could be saved to an excel or csv file (e.g., write.csv)
#> $within
#>          day    var1    var2    var3
#> day        1   -0.01   -0.01 0.12***
#> var1   -0.01       1 0.12*** 0.79***
#> var2   -0.01 0.12***       1 0.29***
#> var3 0.12*** 0.79*** 0.29***       1
#> 
#> $note
#> [1] "***p < 0.001, **p < 0.01, *p < 0.05"
#> 
```
