# Save tables or matrices to Excel

Use `to_excel(get_matrix(wbCorrObject))` or
`to_excel(get_table(wbCorrObject))` to save wbCorr output. A single data
frame or matrix can also be passed directly. Lists may contain any
mixture of data frames and matrices; other list elements, such as
explanatory notes returned by
[`get_matrix()`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md),
are ignored.

## Usage

``` r
to_excel(SummaryObject, path = file.path(getwd(), "wbCorr.xlsx"))
```

## Arguments

- SummaryObject:

  A data frame, matrix, or list containing data frames and/or matrices,
  including objects returned by
  [`get_matrix()`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md)
  or
  [`get_table()`](https://pascal-kueng.github.io/wbCorr/reference/get_table.md).
  Row names are written to a `RowName` column.

- path:

  A single non-missing file path. If omitted, `wbCorr.xlsx` is written
  to the current working directory.

## Value

The output path, invisibly. The function writes an Excel workbook to
disk and errors if no data frame or matrix was supplied.

## See also

[`get_tables`](https://pascal-kueng.github.io/wbCorr/reference/get_table.md),
[`wbCorr`](https://pascal-kueng.github.io/wbCorr/reference/wbCorr.md),
[`get_matrix`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md)

## Examples

``` r
# Importing our simulated example dataset with pre-specified within- and between- correlations
data("simdat_intensive_longitudinal")

# Create object:
correlations <- wbCorr(data = simdat_intensive_longitudinal,
                      cluster = 'participantID')
#> Warning: Analytic p-values and confidence intervals are working approximations for clustered data; use inference = 'cluster_bootstrap' for whole-cluster resampling intervals.

# Returns a correlation matrix with stars for p-values:
matrices <- get_matrix(correlations) # summary(correlations) works too.

to_excel(matrices, path = tempfile(fileext = ".xlsx"))

# A data frame or matrix can be exported directly:
to_excel(data.frame(x = 1:3), path = tempfile(fileext = ".xlsx"))
to_excel(matrix(1:4, nrow = 2), path = tempfile(fileext = ".xlsx"))
```
