#' @title Saves the passed summary or table to excel
#' @description Use `to_excel(get_matrix(wbCorrObject))` or `to_excel(get_table(wbCorrObject))` to save
#' the provided table/matrix to an excel file.
#'
#' @param SummaryObject A summary or matrix object, such as those returned by `get_matrix()` or `get_table()`.
#' @param path Specify the filename and a path. If no path is provided, the file will be saved to the
#' current working directory.
#'
#' @return Writes an Excel file (.xlsx) to disk.
#' @seealso \code{\link[=get_table]{get_tables}}, \code{\link[=wbCorr]{wbCorr}}, \code{\link[=get_matrix]{get_matrix}}
#' @examples
#' # Importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#'
#' # Create object:
#' correlations <- wbCorr(data = simdat_intensive_longitudinal,
#'                       cluster = 'participantID')
#'
#' # Returns a correlation matrix with stars for p-values:
#' matrices <- get_matrix(correlations) # summary(correlations) works too.
#'
#' to_excel(matrices)
#'
#' @export
to_excel <- function(SummaryObject,
                     path = file.path(getwd(), "wbCorr.xlsx")) {

  # Filter only dataframes
  only_dfs <- lapply(SummaryObject, function(x) {
    if (is.data.frame(x)) {
      # Add row names as the first column
      x <- cbind(RowName = rownames(x), x)
      return(x)
    }
  })

  # Remove NULL elements if any were created
  only_dfs <- Filter(Negate(is.null), only_dfs)

  # Create workbook.
  writexl::write_xlsx(only_dfs, path)
}
