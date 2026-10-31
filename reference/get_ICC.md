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

A dataframe with ICCs for all variables. The ICC(1,1) of the one-way
random-effects model is estimated from ANOVA mean squares, using all
available observations of each variable. Sample estimates can be
negative.

## See also

[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)

## Examples

``` r
# importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')

# returns the ICCs:
ICCs <- get_ICC(correlations)
print(ICCs)
#>   variable         ICC
#> 1      day -0.02040816
#> 2     var1  0.51348244
#> 3     var2  0.49354674
#> 4     var3  0.49258895
```
