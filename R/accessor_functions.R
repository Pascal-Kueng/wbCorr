
#' Retrieve within- and/or between-cluster correlations for a wbCorr object (tables).
#' Correlations matrices can be obtained via the `summary()` or `summary.wbCorr()` function.
#'
#' @param object A wbCorr object, created by the `wbCorr()` function.
#' @param which A character vector indicating which correlation table to return.
#' Options are `'within'` or `'w'`, and `'between'` or `'b'`.
#'
#' @return A list containing the selected tables of within- and/or between-cluster correlations.
#'
#' @seealso \code{\link[=summary.wbCorr]{summary}}, \code{\link[=wbCorr]{wbCorr}}
#' @examples
#' # Example using the iris dataset
#' wbCorrObject <- wbCorr(iris, iris$Species)
#' correlations <- get_correlations(wbCorrObject, which = c('within', 'between', 'merge'))
#' correlations$within
#' correlations$between
#'
#' @export
get_correlations <- function(object, which = c('within',
                                               'between')) {
  which <- match.arg(which, choices = c('within', 'w', 'between', 'b'), several.ok = TRUE) # Check for valid inputs
  output_list <- list()
  if ('within' %in% which | 'w' %in% which) {
    output_list[['within']] <- object@within$table
  }
  if ('between' %in% which | 'b' %in% which) {
    output_list[['between']] <- object@between$table
  }
  return(output_list)
}


