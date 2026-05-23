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
#' Supported methods are 'pearson', 'spearman', and 'spearman-jackknife'.
#' (default: 'pearson'). 'pearson': Pearson correlation method uses t-statistics
#' to determine confidence intervals and p-values.'spearman': Spearman correlation
#' method uses the Fisher z-transformation for confidence intervals and p-values.
#' 'spearman-jackknife': Employs the Euclidean jackknife technique to compute
#' confidence intervals, providing more robust confidence intervals in the presence of
#' non-normal data or outliers. Note that p-values are not available
#' when this method is selected.
#' @param bootstrap Performs a bias-corrected and accelerated (BCa) parametric bootstrap to compute confidence
#' intervals and p-values. Recommended for non-normal data, but slow. (default: FALSE).
#' @param nboot Specifies the amount of bootstrap samples (default: 1000).
#' @param weighted_between_statistics Deprecated logical alias for
#' `between_weighting`. If TRUE, `between_weighting = "cluster_size"`; if
#' FALSE, `between_weighting = "equal_clusters"`.
#' @param between_weighting A string specifying the between-cluster estimand.
#' `"equal_clusters"` correlates pair-specific cluster means with each cluster
#' contributing equally. `"cluster_size"` computes a sample-size weighted
#' correlation of pair-specific cluster means, using the number of complete
#' observation pairs in each cluster as weights.
#' @param between_inference A string specifying whether between-cluster
#' p-values and confidence intervals are calculated analytically (`"analytic"`)
#' or omitted (`"none"`). Analytic inference for `"cluster_size"` weighted
#' between correlations uses `k - 2` cluster-level degrees of freedom for
#' Pearson correlations and is approximate.
#' @return A wbCorr object that contains within- and between-cluster statistics.
#' Use the get_table() function on the wbCorr object to retrieve a list of the full correlation tables.
#' Use the summary() or get_matrix() function on the wbCorr object to retrieve various correlation matrices, including ICCs in the merged ones.
#' Use  get_ICC() in order to get all intra class correlations (ICC(1,1)).
#' Finally, use to_excel() on a table or matrix (or list of matrices) to save them.
#'
#' @description
#' The wbCorr function creates a wbCorr object containing within- and
#' between-cluster correlations, p-values, and confidence intervals for a given
#' dataset and clustering variable. The object can be plotted.
#'
#' @details
#' For every variable pair, wbCorr first keeps only rows where both variables
#' and the cluster variable are observed. This means missing data are handled
#' pairwise.
#'
#' The within-cluster correlation is the pooled residual correlation. For a
#' given pair, each observed value is centered around its cluster mean for that
#' same complete-pair row set, and the correlation is computed on the resulting
#' residuals. For Pearson within-cluster correlations, analytic inference uses
#' `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of
#' complete observation pairs and `k_pair` is the number of clusters
#' contributing at least one complete pair.
#'
#' The between-cluster correlation is computed from pair-specific cluster means.
#' With `between_weighting = "equal_clusters"`, every cluster contributes one
#' equally weighted mean. With `between_weighting = "cluster_size"`, cluster
#' means are weighted by the number of complete observation pairs in that
#' cluster. Analytic p-values and confidence intervals for cluster-size weighted
#' between correlations are approximate; use `between_inference = "none"` to
#' report only the weighted coefficient.
#'
#' Inspired by the psych::statsBy function, wbCorr allows you to calculate,
#' extract, and plot within- and between-cluster correlations for further
#' analysis.
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
#' # optionally compute sample-size weighted between-cluster correlations:
#' weighted_correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID',
#'                      between_weighting = 'cluster_size')
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
#' to_excel(matrices)
#'
#'
#' @export
wbCorr <- function(data, cluster,
                   confidence_level = 0.95,
                   method = "pearson",
                   bootstrap = FALSE,
                   nboot = 1000,
                   weighted_between_statistics = NULL,
                   between_weighting = c("equal_clusters", "cluster_size"),
                   between_inference = c("analytic", "none")) {

    # input validation and preparation
  input_data <- data

  if (!is.null(weighted_between_statistics) && missing(between_weighting)) {
    between_weighting <- if (isTRUE(weighted_between_statistics)) {
      "cluster_size"
    } else {
      "equal_clusters"
    }
  }
  between_weighting <- match.arg(between_weighting)
  between_inference <- match.arg(between_inference)
  cluster_size_between <- between_weighting == "cluster_size"

  cluster_var <- input_validation_and_prep(input_data, cluster, method,
                                           cluster_size_between,
                                           bootstrap)
  input_data <- remove_cluster_columns(input_data, cluster, cluster_var)

  cluster <- 'cluster'

  # Split variance into between- and within
  centered_df <- wbCenter(input_data, cluster_var, method,
                          cluster_size_between)

  within_df <- centered_df$within[-1]
  between_df <- centered_df$between[-1]
  input_data_cleaned <- centered_df$input_data_cleaned
  var_type <- centered_df$var_type
  warnings <- centered_df$warnings

  centered_data <- list(within_df = within_df, between_df = between_df)

  if (method == 'auto') {
    auto_type <- TRUE
  } else {
    auto_type <- FALSE
  }

  # Calculate correlations, p-values, and confidence intervals.
  within_cors <- corAndPValues(input_data_cleaned,
                               confidence_level = confidence_level,
                               method = method,
                               auto_type = auto_type,
                               var_type = var_type,
                               warnings = warnings,
                               bootstrap = bootstrap,
                               nboot = nboot,
                               cluster_var = cluster_var,
                               level = 'within')
  between_cors <- corAndPValues(input_data_cleaned,
                                confidence_level = confidence_level,
                                method = method,
                                auto_type = auto_type,
                                var_type = var_type,
                                warnings = warnings,
                                bootstrap = bootstrap,
                                nboot = nboot,
                                cluster_var = cluster_var,
                                level = 'between',
                                between_weighting = between_weighting,
                                between_inference = between_inference)

  within_corr_coefs <- within_cors$correlation_coefficient
  between_corr_coefs <- between_cors$correlation_coefficient

  within_p_values <- within_cors$p_value
  between_p_values <- between_cors$p_value

  within_confidence_intervals <- within_cors$confidence_intervals
  between_confidence_intervals <- between_cors$confidence_intervals

  within_table <- within_cors$result_table
  between_table <- between_cors$result_table


  # Calculate ICCs
  ICC <- compute_ICC1_alt(within_df, input_data)


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
                   bootstrap = bootstrap,
                   nboot = nboot,
                   weighted_between_statistics = cluster_size_between,
                   between_weighting = between_weighting,
                   between_inference = between_inference,
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
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @aliases print.wbCorr
#' @rdname print.wbCorr
#' @examples
#' # Example
#' data("simdat_intensive_longitudinal")
#' correlations <- wbCorr(simdat_intensive_longitudinal,
#'                        cluster = 'participantID',
#'                        confidence_level = 0.95,
#'                        method = 'spearman',
#'                        weighted_between_statistics = FALSE)
#' print(correlations)
#'
#' @importFrom methods setMethod
#' @export
methods::setMethod("print", signature("wbCorr"), function(x, ...) {
  cat("\n---- wbCorr Object ----\n")
  cat("Call: ", deparse(x@call), "\n")
  cat("\nAccess full tables with get_tables(object, which = c('within', 'between'))")
  cat("\nAccess correlation matrices with summary(object, which = c('within', 'between', merge')")
  cat("\nAccess full ICC list with get_ICC(object)\n")

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
  cat("\nAccess correlation matrices with summary(object, which = c('within', 'between', merge')")
  cat("\nAccess full ICC list with get_ICC(object)\n")
})


#######################################################
# Show()
#######################################################

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


#######################################################
# summary()
#######################################################

#' @rdname  get_matrix
#' @aliases get_matrices
#' @aliases summary.wbCorr
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
#' A regression line is provided and the corresponding coefficient with significance displayed.
#' @param x A wbCorr object to be plotted.
#' @param y Choose which correlations to plot ('within' / 'w' or 'between' / 'b'); can be used as a positional argument.
#' @param which Can be used as an alternative to 'y' (e.g., which = 'w'). It has the same functionality as 'y', but takes precedence if both are specified.
#' @param plot_NA Boolean. Whether variables that have no variation on the selected level should be plotted or not.
#' @param standardize Boolean. Whether the dataset should be standardized. If TRUE, the regression coefficient is equivalent to the pearson
#' correlation.
#' @param outlier_detection If FALSE, outliers will not be marked in red. Otherwise you may provide the method. Choose from: 'zscore', 'mad', or 'tukey'.
#' @param outlier_threshold If 'recommended', the threshold for 'zscore' and 'mad' will be set to 3, and for 'tukey' to 1.5. You can provide and other numeric here.
#' @param type points, lines, etc. see ?base::plot for available types).
#' @param pch Graphical parameter. Select which type of points should be plotted.
#' @param dot_lwd Graphical parameter. Set size of the points.
#' @param reg_lwd Graphical parameter. Set thickness of the regression line.
#' @param ... further options to be passed to the base plot (pairs) function.
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @export
#' @aliases plot.wbCorr
methods::setMethod("plot", signature(x = "wbCorr", y = "ANY"), wb_plot)
