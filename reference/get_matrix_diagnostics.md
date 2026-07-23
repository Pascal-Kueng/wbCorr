# Inspect positive-semidefinite diagnostics for correlation matrices

Returns matrix-level diagnostics computed from the unrounded within- and
between-cluster correlation matrices stored in a
[`wbCorr()`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md)
object. Pairwise-complete correlations can be non-positive-semidefinite
when different pairs use different rows. A diagnostic is not assessable
when the corresponding matrix contains an unavailable (`NA`)
coefficient.

## Usage

``` r
get_matrix_diagnostics(object)
```

## Arguments

- object:

  A `wbCorr` object.

## Value

A data frame with one row for each level and columns identifying the
diagnostic status, positive-semidefinite result, minimum eigenvalue,
completeness, numerical tolerance, number of variables, missing-data
mode, whether PSD followed from a common-matrix construction, and reason
when assessment was unavailable.

## See also

[`wbCorr()`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md),
[`get_matrix()`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md)
