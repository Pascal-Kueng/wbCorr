#' wbCorr
#'
#' Calculates bivariate within- and between-cluster correlations for clustered
#' data, such as repeated measures nested in persons, dyads, teams, or other
#' groups. Only recommended for continuous or binary variables.
#'
#' @param data A dataframe containing numeric variables for which correlations will be calculated.
#' @param cluster A vector representing the clustering variable or a string with the name of the column in data that contains the clustering variable.
#' @param confidence_level A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).
#' @param method A string indicating the correlation method to be used.
#' Supported methods are 'pearson', 'spearman', and 'auto' (default: 'pearson').
#' 'pearson': p-values use t-statistics and confidence intervals use the Fisher z-transformation.
#' 'spearman': p-values and confidence intervals use the Fisher z-transformation.
#' 'auto': uses Spearman for pairs with an ordered factor and Pearson otherwise
#' (reported as point-biserial or phi coefficient for binary variables).
#' @param nboot Specifies the amount of bootstrap samples (default: 1000).
#' @param inference A string specifying how p-values and confidence intervals are computed.
#' 'analytic' (default) uses the formulas described under `method`.
#' 'cluster_bootstrap' resamples whole clusters and computes bias-corrected (BC)
#' bootstrap confidence intervals and p-values. Unlike the analytic tests, it
#' accounts for dependence within clusters and does not assume normal data.
#' Use `set.seed()` for reproducible results.
#' 'none' returns the correlation coefficients only.
#' @param between_weighting A string. If 'equal_clusters' (default), every cluster mean
#' is weighted the same in between-cluster correlations, even if some clusters
#' contributed fewer measurement points. If 'cluster_size', cluster means are
#' weighted by the number of observations each cluster has of both variables. These methods will be equivalent in datasets
#' without missing data and an equal number of measurements per cluster.
#' @return A wbCorr object that contains within- and between-cluster statistics.
#' Use the get_table() function on the wbCorr object to retrieve a list of the full correlation tables.
#' Use the summary() or get_matrix() function on the wbCorr object to retrieve various correlation matrices, including ICCs in the merged ones.
#' Use get_ICC() to retrieve all intraclass correlations (ICC(1,1)).
#' Finally, use to_excel() on a table or matrix (or list of matrices) to save them.
#'
#' @description
#' The wbCorr function creates a wbCorr object containing within- and between-cluster correlations,
#' p-values, and confidence intervals for a given dataset and clustering variable. The object can be plotted.
#'
#' @details
#' Inspired by the psych::statsBy function, wbCorr allows you to easily calculate, extract, and plot within-
#' and between-cluster correlations for further analysis.
#'
#' Each variable is split into its cluster means (between-cluster part) and the
#' deviations from these means (within-cluster part). Cluster means use all
#' available observations of a variable. Each correlation then uses all complete
#' pairs of the two variables.
#'
#' Within-cluster tests use N - k - 1 degrees of freedom, where N is the number of
#' complete pairs and k the number of clusters they come from. These tests treat
#' the deviations as independent, which is an approximation. Between-cluster tests
#' use k - 2 degrees of freedom. A correlation is `NA` if there are too few
#' observations, e.g. fewer than three clusters for between-cluster correlations.
#'
#' @seealso
#' \code{\link[=get_table]{get_table}},
#' \code{\link[=summary.wbCorr]{summary}},
#' \code{\link[=get_ICC]{get_ICC}},
#' \code{\link[=plot.wbCorr]{plot}},
#' \code{\link[=to_excel]{to_excel}}
#'
#' @examples
#' # importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#'
#' # create a wbCorr object:
#' correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID')
#'
#' # optionally weight between-cluster correlations by cluster size:
#' weighted_correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID',
#'                      between_weighting = 'cluster_size')
#'
#' # cluster bootstrap; use more bootstrap samples in applied work:
#' \donttest{
#' bootstrapped_correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID',
#'                      inference = 'cluster_bootstrap',
#'                      nboot = 100)
#' }
#'
#' # returns a list with full detailed tables of the correlations:
#' tables <- get_table(correlations) # the get_tables() function is equivalent
#' print(tables)
#'
#' # returns a correlation matrix with stars for p-values:
#' matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
#' print(matrices)
#'
#' # Plot the centered variables against each other
#' plot(correlations, 'within')
#' plot(correlations, which = 'b')
#'
#' # Store the list of correlation matrices to excel
#' to_excel(matrices, path = tempfile(fileext = ".xlsx"))
#'
#'
#' @export
wbCorr <- function(data, cluster,
                   confidence_level = 0.95,
                   method = "pearson",
                   nboot = 1000,
                   inference = c("analytic", "none", "cluster_bootstrap"),
                   between_weighting = c("equal_clusters", "cluster_size")) {

  inference <- match.arg(inference)
  between_weighting <- match.arg(between_weighting)
  weighted_between_statistics <- between_weighting == "cluster_size"

  # input validation and preparation
  prepared <- input_validation_and_prep(data, cluster, method)
  input_data <- prepared$input_data
  cluster_var <- prepared$cluster_var

  # Split variance into between- and within
  centered_df <- wbCenter(input_data, cluster_var, method,
                          weighted_between_statistics)

  within_df <- centered_df$within[-1]
  between_df <- centered_df$between[-1]
  var_type <- centered_df$var_type
  warnings <- centered_df$warnings

  centered_data <- list(within_df = within_df, between_df = between_df)

  if (method == 'auto') {
    auto_type <- TRUE
  } else {
    auto_type <- FALSE
  }

  # Calculate correlations, p-values, and confidence intervals.
  within_cors <- corAndPValues(within_df,
                               cluster = centered_df$within$cluster,
                               level = 'within',
                               confidence_level = confidence_level,
                               method = method,
                               auto_type = auto_type,
                               var_type = var_type,
                               warnings = warnings,
                               inference = inference,
                               nboot = nboot)
  between_cors <- corAndPValues(between_df,
                                cluster = centered_df$between$cluster,
                                level = 'between',
                                confidence_level = confidence_level,
                                method = method,
                                auto_type = auto_type,
                                var_type = var_type,
                                warnings = warnings,
                                inference = inference,
                                nboot = nboot)

  within_corr_coefs <- within_cors$correlation_coefficient
  between_corr_coefs <- between_cors$correlation_coefficient

  within_p_values <- within_cors$p_value
  between_p_values <- between_cors$p_value

  within_confidence_intervals <- within_cors$confidence_intervals
  between_confidence_intervals <- between_cors$confidence_intervals

  within_table <- within_cors$result_table
  between_table <- between_cors$result_table


  # Calculate ICCs
  ICC <- compute_ICC1(centered_df$input_data_cleaned, cluster_var)


  # Store everything in three sections of the object
  within <- list(correlations = within_corr_coefs,
                 p_values = within_p_values,
                 confidence_intervals = within_confidence_intervals,
                 table = within_table)
  between <- list(correlations = between_corr_coefs,
                  p_values = between_p_values,
                  confidence_intervals = between_confidence_intervals,
                  table = between_table)

  # Store settings
  settings <- list(data = data, cluster = cluster,
                   confidence_level = confidence_level,
                   method = method,
                   nboot = nboot,
                   inference = inference,
                   between_weighting = between_weighting,
                   auto_type = auto_type,
                   var_type = var_type)

  output <- new("wbCorr",
                within = within,
                between = between,
                ICC = ICC,
                centered_data = centered_data,
                settings = settings)

  attr(output, "call") <- match.call()
  return(output)
}


#######################################################
# Defining the wbCorr class
#######################################################
#' @importFrom methods show
#' @importFrom methods new
#' @importFrom stats ave complete.cases cor pt
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
methods::setClass("wbCorr", representation(within = "list",
                                           between = "list",
                                           ICC = "data.frame",
                                           centered_data = "list",
                                           settings = 'list'))

#' @rdname wbCorr
#' @export
wbcorr <- wbCorr

#######################################################
# Print()
#######################################################

# Set method for printing
#' @title Print Method for the wbCorr Class
#' @description Prints a summary of the \code{wbCorr} object.
#' @param x A \code{wbCorr} object.
#' @param ... Additional arguments, currently unused.
#' @return Invisibly returns the supplied \code{wbCorr} object. Called for the
#' side effect of printing a compact summary of the within-cluster table,
#' between-cluster table, and ICC table.
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @aliases print.wbCorr
#' @rdname print.wbCorr
#' @examples
#' # Example
#' data("simdat_intensive_longitudinal")
#' correlations <- wbCorr(simdat_intensive_longitudinal,
#'                        cluster = 'participantID',
#'                        confidence_level = 0.95,
#'                        method = 'spearman')
#' print(correlations)
#'
#' @importFrom methods setMethod
#' @export
methods::setMethod("print", signature("wbCorr"), function(x, ...) {
  cat("\n---- wbCorr Object ----\n")
  cat("Call: ", deparse(x@call), "\n")

  # Function for printing a section of the object
  print_section <- function(title, data) {
    cat("\n", title, "\n")
    cat(strrep("-", nchar(title)), "\n")
    print(head(data))
    if (nrow(data) > 6) {
      cat("... ", nrow(data) - 6, " more rows\n")
    }
  }

  # printing...
  print_section("Within-Cluster Correlations:", x@within$table)
  print_section("Between-Cluster Correlations:", x@between$table)
  if (length(x@ICC) > 0) {
    print_section("Intraclass Correlation Coefficients:", x@ICC)
  }

  cat("\nAccess full tables with get_tables(object, which = c('within', 'between'))")
  cat("\nAccess correlation matrices with summary(object, which = c('within', 'between', 'merge'))")
  cat("\nAccess full ICC list with get_ICC(object)\n")
  invisible(x)
})


#######################################################
# Show()
#######################################################

#' @title Show Method for the wbCorr Class
#'
#' @description Shows a summary of the \code{wbCorr} object, equivalent to the print method.
#'
#' @param object A \code{wbCorr} object.
#' @return Invisibly returns the supplied \code{wbCorr} object. Called for the
#' side effect of showing the same compact summary as \code{print()}.
#' @seealso \code{\link[=wbCorr]{wbCorr}}, \code{\link[=print.wbCorr]{print.wbCorr}}
#' @aliases show.wbCorr
#' @rdname show.wbCorr
#' @examples
#' # Example using the iris dataset
#' cors <- wbCorr(iris, iris$Species, between_weighting = 'cluster_size')
#' show(cors)
#' @export
setMethod("show", signature("wbCorr"), function(object) {
  print(object)
})


#######################################################
# summary()
#######################################################

#' @rdname get_matrix
#' @method summary wbCorr
#' @exportS3Method summary wbCorr
summary.wbCorr <- function(object, ...) {
  get_matrices(object, ...)
}

#' @rdname  get_matrix
#' @importFrom methods setMethod
#' @export
methods::setMethod("summary", signature("wbCorr"), get_matrices)


#######################################################
# plot()
#######################################################

#' @title Plot within- and between associations
#' @description Plots the centered variables of the provided dataframe against each other.
#' Choose whether to plot the between-centered variables (representing the between-cluster correlations by plotting cluster means)
#' or the within-centered variables (representing the within-cluster correlations by plotting deviations from person-means).
#' Panels below the diagonal show scatterplots with a regression line. Panels above the diagonal show
#' the correlation from the wbCorr object with its significance stars.
#' @param x A wbCorr object to be plotted.
#' @param y Choose which correlations to plot ('within' / 'w' or 'between' / 'b'); can be used as a positional argument.
#' @param which Can be used as an alternative to 'y' (e.g., which = 'w'). It has the same functionality as 'y', but takes precedence if both are specified.
#' @param plot_NA Boolean. Whether variables that have no variation on the selected level should be plotted or not.
#' @param standardize Boolean. Whether the dataset should be standardized before plotting.
#' @param outlier_detection If FALSE, outliers will not be marked in red. Otherwise you may provide the method. Choose from: 'zscore', 'mad', or 'tukey'.
#' @param outlier_threshold If 'recommended', the threshold for 'zscore' and 'mad' will be set to 3, and for 'tukey' to 1.5. You can provide and other numeric here.
#' @param type points, lines, etc. see ?base::plot for available types).
#' @param pch Graphical parameter. Select which type of points should be plotted.
#' @param dot_lwd Graphical parameter. Set size of the points.
#' @param reg_lwd Graphical parameter. Set thickness of the regression line.
#' @param ... further options to be passed to the base plot (pairs) function.
#' @return Called for the side effect of drawing a pairs plot; returns
#' \code{NULL} invisibly.
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @name plot,wbCorr-method
#' @method plot wbCorr
#' @exportS3Method plot wbCorr
plot.wbCorr <- function(x, y, ...) {
  wb_plot(x, y, ...)
}

#' @rdname plot-wbCorr-method
#' @export
methods::setMethod("plot", signature(x = "wbCorr", y = "ANY"), wb_plot)
