# Saves the passed summary or table to excel

Use `to_excel(get_matrix(wbCorrObject))` or
`to_excel(get_table(wbCorrObject))` to save the provided table/matrix to
an excel file.

## Usage

``` r
to_excel(SummaryObject, path = file.path(getwd(), "wbCorr.xlsx"))
```

## Arguments

- SummaryObject:

  A summary or matrix object, such as those returned by
  [`get_matrix()`](https://pascal-kueng.github.io/wbCorr/reference/get_matrix.md)
  or
  [`get_table()`](https://pascal-kueng.github.io/wbCorr/reference/get_table.md),
  or a single table.

- path:

  Specify the filename and a path. If no path is provided, the file will
  be saved to the current working directory.

## Value

Writes an Excel file (.xlsx) to disk and invisibly returns its path.

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

# Returns a correlation matrix with stars for p-values:
matrices <- get_matrix(correlations) # summary(correlations) works too.

to_excel(matrices, path = tempfile(fileext = ".xlsx"))
```
