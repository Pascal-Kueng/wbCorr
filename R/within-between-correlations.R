#' wbCorr
#'
#' Calculates within- and between-cluster correlations for a given dataset and clustering variable.
#'
#' @param data A dataframe containing numeric variables for which correlations will be calculated.
#' @param cluster A vector representing the clustering variable or a string with the name of the column in `data` that contains the clustering variable.
#' @param alpha_level A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).
#' @param method A string indicating the correlation method to be used. Supported methods are 'pearson', 'kendall', and 'spearman' (default: 'pearson').
#'
#' @return A wbCorr object that contains within- and between-cluster correlations, p-values, and confidence intervals.
#' Use the `get_correlations()` function on the wbCorr object to retrieve full tables.
#' Use the `summary()` function on the wbCorr object to retrieve correlation matrices.
#'
#' @description
#' The `wbCorr` function creates a wbCorr object containing within- and between-cluster correlations,
#' p-values, and confidence intervals for a given dataset and clustering variable.
#'
#' @details
#' Inspired by the psych::statsBy function, `wbCorr` allows you to easily calculate and extract within-
#' and between-cluster correlations for further analysis.
#'
#' @seealso
#' \code{\link[=get_correlations]{get_correlations}}, \code{\link[=summary.wbCorr]{summary}}

#'
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species)
#' cors
#' get_correlations(cors, which = c('within', 'between'))
#' summary(cors, which = c('within', 'between', 'merge'))
#' @export

wbCorr <- function(data, cluster, alpha_level = 0.95, method = "pearson") {
  # Input validation and error handling
  input_data <- data
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (!method %in% c("pearson", "spearman", "kendall")) {
    stop("Invalid correlation method. Choose one of: 'pearson', 'spearman', or 'kendall'")
  }
  cluster <- as.factor(cluster)
  # Split variance into between- and within
  centered_df <- wbCenter(input_data, cluster)

  # Calculate correlations, p-values, and confidence intervals.
  within_cors <- corAndPValues(centered_df$within[-1],
                               alpha_level = alpha_level,
                               method = method)
  between_cors <- corAndPValues(centered_df$between[-1],
                                n_clusters = unique(
                                  nlevels(as.factor(
                                    centered_df$between$cluster))),
                                alpha_level = alpha_level,
                                method = method)

  within_corr_coefs <- within_cors$correlation_coefficient
  between_corr_coefs <- between_cors$correlation_coefficient

  within_p_values <- within_cors$p_value
  between_p_values <- between_cors$p_value

  within_confidence_intervals <- within_cors$confidence_intervals
  between_confidence_intervals <- between_cors$confidence_intervals

  within_table <- within_cors$result_table
  between_table <- between_cors$result_table

  # Store them in two sections of the object
  within <- list(correlations = within_corr_coefs,
                 p_values = within_p_values,
                 confidence_intervals = within_confidence_intervals,
                 table = within_table)
  between <- list(correlations = between_corr_coefs,
                  p_values = between_p_values,
                  confidence_intervals = between_confidence_intervals,
                  table = between_table)

  output <- new("wbCorr", within = within, between = between)
  attr(output, "call") <- match.call()
  return(output)
}


#######################################################
# Defining the wbCorr class
#######################################################
#' @title wbCorr Class
#'
#' @description A class representing within- and between-cluster correlations.
#'
#' @details The \code{wbCorr} class is used to store within- and between-cluster correlations
#' and provides methods for printing and summarizing the correlations.
#'
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @export
setClass("wbCorr", representation(within = "list", between = "list"))


# Set method for printing
#' @title Print Method for the wbCorr Class
#' @description Prints a summary of the \code{wbCorr} object.
#' @param x A \code{wbCorr} object.
#' @param ... Additional arguments, currently unused.
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @aliases print.wbCorr
#' @rdname print.wbCorr
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species)
#' print(cors)
#' @export
setMethod("print", "wbCorr", function(x, ...) {
  cat("\n---- wbCorr Object ----\n")
  cat("Call: ", deparse(x@call), "\n")
  cat("\nAccess full tables with `get_correlations(object, which = c('within', 'between'))`\n")
  cat("Access correlation matrices with `summary(object, which = c('within', 'between', merge')`\n")

  # Function for printing a section of the object
  max_columns <- max(1, floor((getOption("width")) / 1))
  print_section <- function(title, data) {
    cat("\n", title, "\n")
    cat(strrep("-", nchar(title)), "\n")
    printed_data <- head(data[, 1:min(ncol(data), max_columns)])
    print(printed_data)
    if (ncol(data) > max_columns) {
      cat("... ", ncol(data) - max_columns, " more columns\n")
    }
    if (nrow(data) > 6) {
      cat("... ", nrow(data) - 6, " more rows\n")
    }
  }

  # printing...
  print_section("Within-Cluster Correlations:", x@within$table)
  print_section("Between-Cluster Correlations:", x@between$table)

  cat("\nAccess full tables with get_correlations(object, which = c('within', 'between'))")
  cat("\nAccess correlation matrices with summary(object, which = c('within', 'between', merge')\n")

})

# Set method for showing equal to printing
#' @title Show Method for the wbCorr Class
#' @description Shows a summary of the \code{wbCorr} object, equivalent to the print method.
#' @param object A \code{wbCorr} object.
#' @seealso \code{\link[=wbCorr]{wbCorr}}, \code{\link[=print.wbCorr]{print.wbCorr}}
#' @aliases show.wbCorr
#' @rdname show.wbCorr
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species)
#' show(cors)
#' @export
setMethod("show", "wbCorr", function(object) {
  print(object)
})



#' Summarize within- and/or between-cluster correlations for a wbCorr object.
#'
#' @param object A wbCorr object, created by the `wbCorr()` function.
#' @param which A string or a character vector indicating which summaries to return.
#' Options are `'within'` or `'w'`, `'between'` or `'b'`, and various merge options
#' like `'merge'`, `'m'`, `'merge_wb'`, `'wb'`, `'merge_bw'`, `'bw'`.
#' Default is `c('within', 'between', 'merge')`.
#' @param ... Additional arguments passed to the base summary method
#'
#' @return A list containing the selected summaries of within- and/or between-cluster correlations.
#' @seealso \code{\link[=get_correlations]{get_correlations}}, \code{\link[=wbCorr]{wbCorr}}
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species)
#' summaries <- summary(cors, which = c('within', 'between', 'merge'))
#' summaries$within
#' summaries$between
#' summaries$merged_wb
#'
#' @export
#' @aliases summary.wbCorr
#' @rdname  summary.wbCorr
#'
setMethod("summary", "wbCorr", function(object, which = c('within', 'between', 'merge'),...) {
  which <- match.arg(which, choices = c('within', 'w',
                                        'between', 'b',
                                        'merge','m', 'merged',
                                        'merge_bw', 'bw',
                                        'merge_wb', 'wb'), several.ok = TRUE) # Check for valid inputs

  df_summary_within <- summarize_table(
    object@within$p_values,
    object@within$correlations)

  df_summary_between <- summarize_table(
    object@between$p_values,
    object@between$correlations)

  combined_df_wb <- combine_matrices(as.matrix(df_summary_within),
                                     as.matrix(df_summary_between))
  combined_df_bw <- combine_matrices(as.matrix(df_summary_between),
                                     as.matrix(df_summary_within))

  return_list <- list()
  if ('within' %in% which | 'w' %in% which) {
    return_list[['within']] <- df_summary_within
  }
  if ('between' %in% which | 'b' %in% which) {
    return_list[['between']] <- df_summary_between
  }
  if ('merge' %in% which | 'm' %in% which | 'merged' %in% which) {
    return_list[['merged_wb']] <- combined_df_wb
    return_list[['merged_bw']] <- combined_df_bw
  } else if ('merge_bw' %in% which | 'bw' %in% which) {
    return_list[['merged_bw']] <- combined_df_bw
  } else if ('merge_wb' %in% which | 'wb' %in% which) {
    return_list[['merged_wb']] <- combined_df_wb
  }
  return(return_list)
})



