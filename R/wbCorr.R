#' wbCorr
#'
#' Calculates within- and between-cluster correlations for a given dataset and clustering variable.
#'
#' @param data A dataframe containing numeric variables for which correlations will be calculated.
#' @param cluster A vector representing the clustering variable or a string with the name of the column in `data` that contains the clustering variable.
#' @param alpha_level A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).
#' @param method A string indicating the correlation method to be used. Supported methods are 'pearson' and 'spearman' (default: 'pearson').
#' @param weighted_between_statistics A logical value. If False, Variables are centered between-persons by
#' simply taking the mean for each person and weighting them all the same, even if some
#' contributed less measurement points. If TRUE, correlations are weighted. These methods will be equivalent in datasets
#' without missing data and an equal amount of measurements per person. TRUE only supports continuous variables (default: FALSE).
#' @return A wbCorr object that contains within- and between-cluster correlations, p-values, and confidence intervals.
#' Use the `get_table()` function on the wbCorr object to retrieve full tables.
#' Use the `summary()` or `get_matrix()` function on the wbCorr object to retrieve correlation matrices.
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
#' \code{\link[=get_table]{get_table}}, \code{\link[=summary.wbCorr]{summary}}, \code{\link[=get_matrix]{get_matrix}}
#'
#' @examples
#' # importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#' # use ?simdat_intensive_longitudinal # documentation of the dataset and the "true" correlations
#' head(simdat_intensive_longitudinal)
#'
#' # returs an object:
#' correlations <- wbCorr(data = simdat_intensive_longitudinal,
#'                       cluster = 'participantID')
#'
#' # returns a list with full detailed tables of the correlations:
#' tables <- get_table(correlations) # the get_tables() function is equivalent
#' print(tables)
#'
#' # returns a correlation matrix with stars for p-values:
#' matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
#' print(matrices)
#'
#' # Access specific tables or matrices by:
#' # Option 1:
#' matrices$within
#' tables$between
#' # Option 2:
#' get_tables(correlations, which = 'within')
#' summary(correlations, which = c('w', 'wb', 'bw')) # abbreviations equivalent to full words
#'
#' @export
wbCorr <- function(data, cluster,
                   alpha_level = 0.95, method = "pearson",
                   weighted_between_statistics = TRUE) {
  # Input validation and error handling
  input_data <- data
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (!method %in% c("pearson", "spearman")) {
    stop("Invalid correlation method. Choose one of: 'pearson' or 'spearman'")
  }

  # Split variance into between- and within
  centered_df <- wbCenter(input_data, cluster, weighted_between_statistics)

  # Calculate correlations, p-values, and confidence intervals.
  within_cors <- corAndPValues(centered_df$within[-1],
                               alpha_level = alpha_level,
                               method = method)
  between_cors <- corAndPValues(centered_df$between[-1],
                                n_clusters_between = nlevels(as.factor(
                                    centered_df$between$cluster)),
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
#' @importFrom methods show
#' @importFrom methods new
#' @importFrom stats ave complete.cases cor pt qt
#' @importFrom utils combn head
#' @title wbCorr Class
#'
#' @description A class representing within- and between-cluster correlations.
#'
#' @details The \code{wbCorr} class is used to store within- and between-cluster correlations
#' and provides methods for printing and summarizing the correlations.
#'
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @importFrom methods setMethod
#' @importFrom methods setClass
#' @export
methods::setClass("wbCorr", representation(within = "list", between = "list"))


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
#' cors <- wbCorr(iris, iris$Species, weighted_between_statistics = TRUE)
#' print(cors)
#' @importFrom methods setMethod
#' @export
methods::setMethod("print", "wbCorr", function(x, ...) {
  cat("\n---- wbCorr Object ----\n")
  cat("Call: ", deparse(x@call), "\n")
  cat("\nAccess full tables with `get_tables(object, which = c('within', 'between'))`\n")
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

  cat("\nAccess full tables with get_tables(object, which = c('within', 'between'))")
  cat("\nAccess correlation matrices with summary(object, which = c('within', 'between', merge')\n")

})

#' @title Show Method for the wbCorr Class
#'
#' @description Shows a summary of the \code{wbCorr} object, equivalent to the print method.
#'
#' @param object A \code{wbCorr} object.
#' @seealso \code{\link[=wbCorr]{wbCorr}}, \code{\link[=print.wbCorr]{print.wbCorr}}
#' @aliases show.wbCorr
#' @rdname show.wbCorr
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species, weighted_between_statistics = TRUE)
#' show(cors)
#' @export
setMethod("show", signature("wbCorr"), function(object) {
  print(object)
})

#' @rdname  get_matrix
#' @aliases get_matrices
#' @aliases summary.wbCorr
#' @importFrom methods setMethod
#' @export
methods::setMethod("summary", "wbCorr", get_matrices)


