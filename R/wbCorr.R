#' wbCorr
#'
#' Calculates bivariate within- and between-cluster correlations for clustered
#' data, such as repeated measures nested in persons, dyads, teams, or other
#' groups. Only recommended for continuous or binary variables.
#'
#' @param data A data frame containing numeric variables, logical variables, or
#' two-level factors for which correlations will be calculated.
#' @param cluster An atomic vector with exactly one value per data row, or one
#' string naming the cluster column in `data`. Missing identifiers are allowed,
#' but at least one identifier must be observed; numeric identifiers cannot be
#' `Inf`, `-Inf`, or `NaN`. When a vector is supplied, data columns containing
#' the same identifiers and missing-value pattern are treated as duplicate
#' cluster columns and excluded; pass a column name when the identifier is
#' already in `data` to avoid ambiguity.
#' @param confidence_level A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).
#' @param method A string indicating the correlation method to be used.
#' Supported methods are `"pearson"`, `"spearman"`, and `"auto"` (default:
#' `"pearson"`). Pearson uses t statistics and Fisher-z confidence intervals.
#' Spearman reports a descriptive correlation of mean-centered scores within
#' clusters and a correlation of cluster means between clusters. Analytic
#' Spearman inference is unavailable; use `inference = "cluster_bootstrap"` for
#' a whole-cluster bootstrap interval. The former `"spearman-jackknife"` option
#' is rejected because deleting individual rows does not respect clustering.
#' @param bootstrap Deprecated logical alias for
#' `inference = "cluster_bootstrap"`.
#' @param nboot A whole number of bootstrap samples, at least 10 (default:
#' 1000). The minimum permits quick tests; use substantially more replicates
#' for substantive analyses and assess Monte Carlo stability.
#' @param inference A string specifying inferential output. `"analytic"` uses
#' the documented correlation-test approximation for Pearson p-values and
#' confidence intervals. `"none"` returns coefficients only.
#' `"cluster_bootstrap"` resamples top-level clusters with replacement,
#' recomputes the full decomposition, and reports percentile confidence
#' intervals. Bootstrap p-values are not reported.
#' @param weighted_between_statistics Deprecated logical alias for
#' `between_weighting`. If TRUE, `between_weighting = "cluster_size"`; if
#' FALSE, `between_weighting = "equal_clusters"`.
#' @param between_weighting A string specifying the between-cluster estimand.
#' `"equal_clusters"` correlates pair-specific cluster means with each cluster
#' contributing equally. `"cluster_size"` computes a sample-size weighted
#' correlation of pair-specific cluster means, using the number of complete
#' observation pairs in each cluster as weights. Cluster-size weighting is not
#' supported with `method = "spearman"` because no weighted-rank estimand is
#' currently defined.
#' @param between_inference A string specifying whether between-cluster
#' p-values and confidence intervals are calculated analytically (`"analytic"`)
#' or omitted (`"none"`). Analytic inference is unavailable for
#' `between_weighting = "cluster_size"`; wbCorr preserves the weighted
#' coefficient but omits its p-value and confidence interval. Use
#' `inference = "cluster_bootstrap"` for weighted between-cluster inference.
#' Ignored when `inference = "none"` or `inference = "cluster_bootstrap"`.
#' @param centering_rows A string specifying which rows are used to estimate
#' cluster means for within- and between-cluster decomposition.
#' `"pairwise_complete"` uses only rows where both variables in the current pair
#' are observed. `"all_available"` estimates each variable's cluster mean from
#' all available rows for that variable, then correlates the pair on complete
#' rows.
#' @return A wbCorr object that contains within- and between-cluster statistics.
#' Use the get_table() function on the wbCorr object to retrieve a list of the full correlation tables.
#' Use the summary() or get_matrix() function on the wbCorr object to retrieve various correlation matrices, including ICCs in the merged ones.
#' Use get_ICC() to retrieve all intraclass correlations (ICC(1,1)).
#' Finally, use to_excel() on a table or matrix (or list of matrices) to save them.
#'
#' @description
#' The wbCorr function creates a wbCorr object containing within- and
#' between-cluster correlations, p-values, and confidence intervals for a given
#' dataset and clustering variable. The object can be plotted.
#'
#' @details
#' Logical variables are encoded as 0/1. Factors must declare exactly two
#' levels and are encoded as 0/1 in declared factor-level order, so reversing
#' the levels reverses correlations with other variables. Character variables
#' must first be converted to factors with an explicit two-level order. Other
#' factors are not accepted; use meaningful numeric scores for ordered
#' variables or dummy-code nominal variables. Numeric `Inf`, `-Inf`, and `NaN`
#' values are treated as missing before centering and estimation.
#'
#' For every variable pair, correlations are computed on rows where both
#' variables and the cluster variable are observed. By default,
#' `centering_rows = "pairwise_complete"` also estimates cluster means from this
#' same complete-pair row set. This keeps the within residuals centered for the
#' actual pairwise sample and makes the between correlation a correlation of
#' matched pair-specific cluster means.
#'
#' Detailed tables always retain one row for every requested unordered pair.
#' `n_obs` is the number of jointly observed raw rows with a nonmissing cluster
#' identifier, and `n_clusters` is the number of clusters contributing at least
#' one such row. Under `centering_rows = "all_available"`, additional unpaired
#' rows can contribute to the variable-specific means, but `n_obs` remains the
#' joint pair-row count. `status` describes coefficient estimability (`"ok"` or
#' `"not_estimable"`) and `reason` gives a stable failure code.
#' `inference_status` separately records `"not_requested"`, `"ok"`,
#' `"partial"`, or `"unavailable"`, with details in `inference_reason`.
#' A descriptive coefficient requires two analysis units with positive
#' variance; inferential output can require more.
#'
#' With `centering_rows = "all_available"`, each variable's cluster mean is
#' estimated from all available rows for that variable before the pairwise
#' correlation is computed. This can make the cluster means more stable when
#' data are missing. It also mirrors a common multilevel-model preprocessing
#' workflow, where person means are often created before the model applies
#' complete-case filtering. That workflow is defensible in multilevel models.
#' In wbCorr, however, the variables are treated symmetrically as a descriptive
#' bivariate decomposition, so all-available centering means the two cluster
#' means in a pair may be based on different occasions. For that reason,
#' `"pairwise_complete"` is the default.
#'
#' The within-cluster correlation is the pooled residual correlation. For a
#' given pair, each observed value is centered around its cluster mean for that
#' same complete-pair row set, and the correlation is computed on the resulting
#' residuals. For Pearson within-cluster correlations, analytic inference uses
#' `N_pair - k_pair - 1` degrees of freedom, where `N_pair` is the number of
#' complete observation pairs and `k_pair` is the number of clusters
#' contributing at least one complete pair. This analytic test is a working
#' approximation because residual pairs can still be dependent within clusters.
#' For resampling intervals that preserve the top-level dependence, use
#' `inference = "cluster_bootstrap"`.
#'
#' The between-cluster correlation is computed from pair-specific cluster means.
#' With `between_weighting = "equal_clusters"`, every cluster contributes one
#' equally weighted mean. With `between_weighting = "cluster_size"`, cluster
#' means are weighted by the number of complete observation pairs in that
#' cluster. The ordinary Pearson t test and Fisher-z interval are not valid for
#' a weighted correlation. Therefore analytic p-values and confidence intervals
#' are omitted for cluster-size-weighted between correlations. Use
#' `inference = "cluster_bootstrap"` when inference is required.
#'
#' Pearson analytic confidence intervals use Fisher's z transformation. If
#' `df` denotes the corresponding t-test degrees of freedom, the Fisher-z
#' standard error is `1 / sqrt(df - 1)`. The interval is unavailable when
#' `df <= 1`.
#'
#' With `method = "spearman"`, the within coefficient is Spearman's correlation
#' of the pairwise mean-centered scores and the between coefficient is
#' Spearman's correlation of the pairwise cluster means. These are descriptive
#' mean-based decompositions, not the conditional-ridit and median-centroid
#' clustered rank parameters of Tu, Li, and Shepherd (2025), and need not be
#' invariant to monotone transformations of the original observations. wbCorr
#' therefore does not attach analytic p-values or confidence intervals to these
#' coefficients. Whole-cluster bootstrap confidence intervals are available.
#'
#' For each variable, wbCorr also reports the one-way random-effects,
#' single-measure ICC(1,1). It is estimated from all finite observations for
#' that variable using the ANOVA method of moments and an effective cluster size
#' when clusters are unbalanced. Negative sample ICCs are retained; they occur
#' when the between-cluster mean square is smaller than the within-cluster mean
#' square and, for severely unbalanced samples, the raw ANOVA estimate can be
#' less than -1. Its population interpretation assumes the one-way random-effects
#' model: independent clusters, a common within-cluster variance, and
#' noninformative cluster size and missingness. The ICC is `NA` when the data do
#' not contain enough clusters or within-cluster replication, or when total
#' variability is zero.
#'
#' With `inference = "cluster_bootstrap"`, wbCorr resamples whole top-level
#' clusters, recomputes the selected within- and between-cluster correlations,
#' and reports first-order percentile bootstrap confidence intervals. This
#' keeps the package's descriptive estimands while avoiding row-level
#' independence assumptions. Interval accuracy assumes independent clusters
#' and adequate numbers of clusters and bootstrap replicates; the technical
#' minimum of 10 valid replicates is not a recommendation for substantive
#' analyses. `n_boot_attempted` and `n_boot_valid` report Monte Carlo yield.
#' Bootstrap is skipped when fewer than three clusters contribute. An interval
#' is unavailable with fewer than 10 finite replicate coefficients, and is
#' marked `"partial"` when invalid replicates had to be excluded. No bootstrap
#' p-value is reported because wbCorr does not currently implement a validated
#' null-resampling test for these clustered estimands.
#'
#' Correlation-matrix diagonals are 1 only when the variable has at least two
#' usable values and positive variance at that level; otherwise they are `NA`.
#' P-value diagonals are always `NA`. Merged summary matrices continue to show
#' the variable's ICC on the diagonal instead of a level-specific self-
#' correlation.
#'
#' Inspired by the psych::statsBy function, wbCorr allows you to calculate,
#' extract, and plot within- and between-cluster correlations for further
#' analysis.
#'
#' @references Tu, S., Li, C., & Shepherd, B. E. (2025). Between- and
#' within-cluster Spearman rank correlations. *Statistics in Medicine*.
#' \doi{10.1002/sim.10326}
#'
#' Hall, P., & Wilson, S. R. (1991). Two guidelines for bootstrap hypothesis
#' testing. *Biometrics, 47*(2), 757-762. \doi{10.2307/2532163}
#'
#' Martin, M. A. (2007). Bootstrap hypothesis testing for some common
#' statistical problems: A critical evaluation of size and power properties.
#' *Computational Statistics & Data Analysis, 51*(12), 6321-6342.
#' \doi{10.1016/j.csda.2007.01.020}
#'
#' Andrews, D. W. K., & Buchinsky, M. (2000). A three-step method for choosing
#' the number of bootstrap repetitions. *Econometrica, 68*(1), 23-51.
#' \doi{10.1111/1468-0262.00092}
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
#' # quick cluster-bootstrap example; use more bootstrap samples in applied work:
#' \donttest{
#' bootstrapped_correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID',
#'                      inference = 'cluster_bootstrap',
#'                      nboot = 20)
#' }
#'
#' # optionally estimate cluster means from all rows available for each variable:
#' all_available_correlations <- wbCorr(simdat_intensive_longitudinal,
#'                      'participantID',
#'                      centering_rows = 'all_available')
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
                   bootstrap = FALSE,
                   nboot = 1000,
                   inference = c("analytic", "none", "cluster_bootstrap"),
                   weighted_between_statistics = NULL,
                   between_weighting = c("equal_clusters", "cluster_size"),
                   between_inference = c("analytic", "none"),
                   centering_rows = c("pairwise_complete", "all_available")) {

  inference_missing <- missing(inference)
  between_weighting_missing <- missing(between_weighting)
  between_inference_missing <- missing(between_inference)
  centering_rows_missing <- missing(centering_rows)

  # input validation and preparation
  input_data <- data
  validate_wbcorr_inputs(input_data,
                          cluster,
                          confidence_level,
                          method,
                          bootstrap,
                          nboot,
                          weighted_between_statistics)
  nboot <- as.integer(nboot)

  legacy_bootstrap_requested <- isTRUE(bootstrap)
  if (legacy_bootstrap_requested && inference_missing) {
    inference <- "cluster_bootstrap"
    warning("bootstrap = TRUE is deprecated; using inference = 'cluster_bootstrap'.",
            call. = FALSE)
  }

  inference <- resolve_wbcorr_choice(inference,
                                     c("analytic", "none", "cluster_bootstrap"),
                                     "inference",
                                     inference_missing && !legacy_bootstrap_requested)
  requested_inference <- inference
  if (legacy_bootstrap_requested &&
      !inference_missing &&
      inference != "cluster_bootstrap") {
    warning("bootstrap = TRUE is deprecated and ignored because inference is not 'cluster_bootstrap'.",
            call. = FALSE)
  }
  if (!is.null(weighted_between_statistics) && between_weighting_missing) {
    between_weighting <- if (isTRUE(weighted_between_statistics)) {
      "cluster_size"
    } else {
      "equal_clusters"
    }
  }
  between_weighting <- resolve_wbcorr_choice(
    between_weighting,
    c("equal_clusters", "cluster_size"),
    "between_weighting",
    between_weighting_missing && is.null(weighted_between_statistics)
  )
  between_inference <- resolve_wbcorr_choice(
    between_inference,
    c("analytic", "none"),
    "between_inference",
    between_inference_missing
  )
  centering_rows <- resolve_wbcorr_choice(
    centering_rows,
    c("pairwise_complete", "all_available"),
    "centering_rows",
    centering_rows_missing
  )

  if (inference == "cluster_bootstrap" && method == "spearman-jackknife") {
    stop("Use method = 'spearman' with inference = 'cluster_bootstrap'.")
  }

  if (method == "spearman-jackknife") {
    stop("method = 'spearman-jackknife' is not supported for clustered data; use method = 'spearman' with inference = 'cluster_bootstrap'.",
         call. = FALSE)
  }
  if (method == "spearman" && between_weighting == "cluster_size") {
    stop("between_weighting = 'cluster_size' is not supported with method = 'spearman'; use equal-cluster Spearman or Pearson with an explicitly chosen weighting.",
         call. = FALSE)
  }
  if (method == "spearman" && inference == "analytic") {
    warning("Analytic inference is not supported for wbCorr's descriptive clustered Spearman coefficients; returning coefficients only. Use inference = 'cluster_bootstrap' for whole-cluster bootstrap intervals.",
            call. = FALSE)
    inference <- "none"
  } else if (inference == "analytic") {
    warning("Analytic p-values and confidence intervals are working approximations for clustered data; use inference = 'cluster_bootstrap' for whole-cluster resampling intervals.",
            call. = FALSE)
  }

  weighted_analytic_requested <- inference == "analytic" &&
    between_weighting == "cluster_size" &&
    between_inference == "analytic"

  if (weighted_analytic_requested) {
    warning("Analytic inference is not supported for cluster-size-weighted between correlations; returning the weighted coefficient without a p-value or confidence interval. Use inference = 'cluster_bootstrap' for weighted inference.",
            call. = FALSE)
    between_inference <- "none"
  }

  cluster_size_between <- between_weighting == "cluster_size"

  cluster_var <- input_validation_and_prep(input_data, cluster, method,
                                           cluster_size_between,
                                           inference == "cluster_bootstrap")
  input_data <- remove_cluster_columns(input_data, cluster)

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
                               level = 'within',
                               centering_rows = centering_rows,
                               inference = inference,
                               requested_inference = requested_inference)
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
                                between_inference = between_inference,
                                weighted_analytic_requested = weighted_analytic_requested,
                                centering_rows = centering_rows,
                                inference = inference,
                                requested_inference = requested_inference)

  within_corr_coefs <- within_cors$correlation_coefficient
  between_corr_coefs <- between_cors$correlation_coefficient

  within_p_values <- within_cors$p_value
  between_p_values <- between_cors$p_value

  within_confidence_intervals <- within_cors$confidence_intervals
  between_confidence_intervals <- between_cors$confidence_intervals

  within_table <- within_cors$result_table
  between_table <- between_cors$result_table


  # Calculate ICCs
  ICC <- compute_icc1(input_data_cleaned, cluster_var)


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
                   inference = inference,
                   requested_inference = requested_inference,
                   weighted_between_statistics = cluster_size_between,
                   between_weighting = between_weighting,
                   between_inference = between_inference,
                   centering_rows = centering_rows,
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
#' @description Plots the centered variables of the provided data frame against
#' each other. Choose either cluster means (`"between"`) or deviations from
#' cluster means (`"within"`). Pearson plots include a regression line and
#' slope; Spearman plots report rho without a linear-regression overlay.
#' Significance stars are shown only when the fitted wbCorr object contains a
#' p-value for that pair.
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
#' @return Invisibly returns the supplied \code{wbCorr} object. Called for the
#' side effect of drawing a pairs plot of the selected within- or
#' between-cluster centered variables.
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @export
#' @aliases plot.wbCorr
methods::setMethod("plot", signature(x = "wbCorr", y = "ANY"), wb_plot)
