#' @title Save tables or matrices to Excel
#' @description Use `to_excel(get_matrix(wbCorrObject))` or
#' `to_excel(get_table(wbCorrObject))` to save wbCorr output. A single data
#' frame or matrix can also be passed directly. Lists may contain any mixture
#' of data frames and matrices; other list elements, such as explanatory notes
#' returned by [get_matrix()], are ignored.
#'
#' @param SummaryObject A data frame, matrix, or list containing data frames
#' and/or matrices, including objects returned by [get_matrix()] or
#' [get_table()]. Row names are written to a `RowName` column.
#' @param path A single non-missing file path. If omitted, `wbCorr.xlsx` is
#' written to the current working directory.
#'
#' @return The output path, invisibly. The function writes an Excel workbook to
#' disk and errors if no data frame or matrix was supplied.
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
#' to_excel(matrices, path = tempfile(fileext = ".xlsx"))
#'
#' # A data frame or matrix can be exported directly:
#' to_excel(data.frame(x = 1:3), path = tempfile(fileext = ".xlsx"))
#' to_excel(matrix(1:4, nrow = 2), path = tempfile(fileext = ".xlsx"))
#'
#' @export
to_excel <- function(SummaryObject,
                     path = file.path(getwd(), "wbCorr.xlsx")) {
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) {
    stop("path must be one non-empty, non-missing character value.",
         call. = FALSE)
  }

  sheets <- prepare_excel_sheets(SummaryObject)
  writexl::write_xlsx(sheets, path)
  invisible(path)
}


prepare_excel_sheets <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    return(list(Sheet1 = excel_data_frame(x)))
  }

  if (!is.list(x)) {
    stop("SummaryObject must be a data frame, matrix, or a list containing at least one data frame or matrix.",
         call. = FALSE)
  }

  supported <- vapply(x,
                      function(element) {
                        is.data.frame(element) || is.matrix(element)
                      },
                      logical(1))
  if (!any(supported)) {
    stop("SummaryObject must contain at least one data frame or matrix.",
         call. = FALSE)
  }

  sheets <- lapply(x[supported], excel_data_frame)
  sheet_names <- names(x)[supported]
  if (is.null(sheet_names)) {
    sheet_names <- rep("", length(sheets))
  }
  missing_names <- is.na(sheet_names) | !nzchar(sheet_names)
  sheet_names[missing_names] <- paste0(
    "Sheet",
    which(missing_names)
  )
  names(sheets) <- make.unique(sheet_names, sep = "_")
  sheets
}


excel_data_frame <- function(x) {
  if (is.matrix(x)) {
    x <- as.data.frame(x, check.names = FALSE)
  }

  data.frame(RowName = rownames(x),
             x,
             check.names = FALSE,
             stringsAsFactors = FALSE)
}
