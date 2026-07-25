#' wbCorr
#'
#' Calculates bivariate level-specific correlations for clustered data,
#' including observations nested in one cluster level and strictly nested
#' three-level structures. Only recommended for continuous or binary
#' variables.
#'
#' @param data A data frame containing numeric variables, logical variables, or
#' two-level factors for which correlations will be calculated.
#' @param cluster For a two-level analysis, an atomic vector with exactly one
#' value per data row or one string naming the cluster column in `data`. For a
#' strictly nested three-level analysis, a fully named list of exactly two
#' scalar column names ordered lower-to-higher, for example
#' `list(person = "person_id", dyad = "dyad_id")`. List names label the
#' grouping levels; list values identify columns in `data`. Three-level
#' hierarchy IDs must be globally unique at the lower level and each lower ID
#' must map to exactly one upper ID. Missing two-level identifiers are allowed,
#' but at least one identifier must be observed; numeric identifiers cannot be
#' `Inf`, `-Inf`, or `NaN`. When an external two-level vector is supplied, data
#' columns containing the same identifiers and missing-value pattern are
#' treated as duplicate cluster columns and excluded; pass a column name when
#' the identifier is already in `data` to avoid ambiguity.
#' @param confidence_level A numeric value between 0 and 1 representing the desired level of confidence for confidence intervals (default: 0.95).
#' @param method A string indicating the correlation method to be used.
#' Supported methods are `"pearson"`, `"spearman"`, and `"auto"` (default:
#' `"pearson"`). Pearson uses t statistics and Fisher-z confidence intervals.
#' Spearman reports a descriptive correlation of mean-centered scores within
#' clusters and a correlation of cluster means between clusters. Analytic
#' Spearman inference is unavailable; use `inference = "cluster_bootstrap"` for
#' a whole-cluster bootstrap interval. The former `"spearman-jackknife"` option
#' is rejected because deleting individual rows does not respect clustering.
#' Three-level decomposition currently supports only `"pearson"`.
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
#' intervals. Bootstrap p-values are not reported. When this argument is
#' omitted, two-level Pearson output defaults to `"analytic"` and three-level
#' output defaults to `"none"`.
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
#' @param missing_data A string specifying whether correlations use all
#' available pairs (`"pairwise"`, the default) or first retain only rows that
#' are complete across every supported analysis variable and the cluster
#' identifier (`"listwise"`). Listwise deletion provides a common multivariate
#' sample for users who require a coherent correlation matrix, at the cost of
#' discarding partially observed rows.
#' @return A `wbCorr` object containing within- and between-cluster statistics
#' for a two-level analysis, or a `wbCorrNested` object containing separate
#' `level1`, `level2`, and `level3` sections for a three-level analysis. Use
#' [get_table()] for full tables and [summary()] or [get_matrix()] for matrices.
#' Merged matrices and [get_ICC()] are available only for two-level objects.
#' Use [to_excel()] on a table, matrix, or list returned by an accessor.
#'
#' @description
#' The `wbCorr()` function creates either a two-level `wbCorr` object or a
#' three-level `wbCorrNested` object containing level-specific correlations and
#' the requested inferential output. Both object types can be inspected,
#' extracted, exported, and plotted.
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
#' By default, `missing_data = "pairwise"`, and every variable-pair correlation
#' is computed on rows where both variables and the cluster variable are
#' observed. Because different pairs can then use different rows, a completed
#' pairwise correlation matrix need not be positive semidefinite. wbCorr checks
#' both unrounded level-specific matrices, warns when a matrix is not positive
#' semidefinite, and exposes the result through [get_matrix_diagnostics()].
#'
#' With `missing_data = "listwise"`, rows missing any supported analysis
#' variable or the cluster identifier are removed before correlation
#' decomposition and inference. This gives all pairs a common raw-row sample.
#' For a two-level object, ICCs keep their documented variable-wise finite-row
#' samples and are not changed by this matrix-oriented option.
#' With a single coefficient method (`"pearson"` or `"spearman"`) and common
#' weights, the resulting complete correlation matrix is positive
#' semidefinite up to numerical tolerance. `method = "auto"` can mix Pearson
#' and Spearman entries, so listwise deletion alone cannot guarantee that its
#' combined matrix is positive semidefinite; diagnostics are still reported.
#'
#' Under pairwise handling,
#' `centering_rows = "pairwise_complete"` also estimates cluster means from this
#' same complete-pair row set. This keeps the within residuals centered for the
#' actual pairwise sample and makes the between correlation a correlation of
#' matched pair-specific cluster means.
#'
#' A fully named two-element list selects the strictly nested three-level mode.
#' For each variable pair, `level1` is the pooled Pearson correlation of
#' observation deviations from their lower-unit means. `level2` is the
#' correlation of lower-unit means after subtracting the mean of the
#' contributing lower-unit means in their upper unit. `level3` is the
#' correlation of those upper-unit means. Thus observations contribute equally
#' at level 1, lower units contribute equally at level 2 and when constructing
#' level-3 means, and upper units contribute equally at level 3. All means are
#' estimated from the same pair-specific complete rows. Correlations across
#' levels are distinct estimands and are not additive. With unequal numbers of
#' observations per lower unit, the equal-lower-unit upper mean deliberately
#' differs from the observation-weighted upper mean used in conventional
#' manifest contextual-model decompositions; the two coincide when lower-unit
#' observation counts are balanced. These are descriptive correlations, not
#' estimates of the cited papers' regression, R-squared, or latent parameters.
#'
#' Three-level IDs must describe a strict global nesting. A lower ID reused
#' under more than one upper ID is rejected; construct an explicit globally
#' unique composite lower ID first. Rows with both hierarchy IDs missing are
#' excluded, whereas a row with only part of its hierarchy path missing is an
#' error. In the first three-level implementation, only
#' `method = "pearson"`, equal-unit weighting,
#' `centering_rows = "pairwise_complete"`, and `inference = "none"` or
#' `"cluster_bootstrap"` are supported. When `inference` is omitted,
#' three-level output defaults to coefficients only. The bootstrap resamples
#' intact upper units and re-keys every duplicated upper unit and its
#' descendants. An interval is attempted only when at least three upper units
#' contribute to the observed variable pair. Every upper unit with a complete
#' hierarchy path then remains in the sampling frame even when it has no
#' complete observations for that pair; such draws can reduce `n_boot_valid`.
#'
#' A `wbCorrNested` object exposes all three levels through `get_table()`,
#' `get_matrix()`, `summary()`, `get_matrix_diagnostics()`, and `plot()` using
#' selectors `"level1"`/`"l1"`, `"level2"`/`"l2"`, and
#' `"level3"`/`"l3"`. It does not provide a merged-triangle matrix or silently
#' reuse the one-way ICC. Level-specific variance-partition coefficients require
#' separately defined variance estimands and are outside this version's scope.
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
#' variance; inferential output can require more. Three-level tables replace
#' `n_clusters` with `n_units`, `n_level2`, and `n_level3`, while retaining the
#' same status fields. `n_units` is the number of values correlated at the
#' selected level: `n_obs` at level 1, `n_level2` at level 2, and `n_level3` at
#' level 3. `n_level2` and `n_level3` count the represented lower and upper
#' units in the pair-complete sample. `n_informative_units` counts lower units
#' with at least two complete observations at level 1, upper units with at
#' least two contributing lower units at level 2, and contributing upper units
#' at level 3.
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
#' For each variable in a two-level object, wbCorr also reports the one-way
#' random-effects, single-measure ICC(1,1). It is estimated from all finite observations for
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
#' clusters, recomputes the selected level-specific correlations, and reports
#' first-order percentile bootstrap confidence intervals. This
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
#' P-value diagonals are always `NA`. For two-level objects, merged summary
#' matrices continue to show the variable's ICC on the diagonal instead of a
#' level-specific self-correlation.
#' A matrix with any unavailable coefficient cannot be assessed as a complete
#' positive-semidefinite correlation matrix and is reported as
#' `"not_assessable"` rather than silently treated as valid.
#'
#' Inspired by the psych::statsBy function, wbCorr allows you to calculate,
#' extract, and plot level-specific correlations for further analysis.
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
#' Rights, J. D., & Sterba, S. K. (2023). R-squared measures for multilevel
#' models with three or more levels. *Multivariate Behavioral Research, 58*(2),
#' 340-367. \doi{10.1080/00273171.2021.1985948}
#'
#' Kerkhoff, D., & Nussbeck, F. W. (2023). Estimation quality and required
#' sample sizes in three-level contextual analysis models. *Methodology, 19*(2),
#' 133-151.
#' \doi{10.5964/meth.9775}
#'
#' Field, C. A., & Welsh, A. H. (2007). Bootstrapping clustered data.
#' *Journal of the Royal Statistical Society: Series B, 69*(3), 369-390.
#' \doi{10.1111/j.1467-9868.2007.00593.x}
#'
#' R Core Team. Correlation, variance and covariance matrices. R statistical
#' software documentation. \url{https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html}
#'
#' @seealso
#' \code{\link[=get_table]{get_table}},
#' \code{\link[=summary.wbCorr]{summary}},
#' \code{\link[=get_matrix_diagnostics]{get_matrix_diagnostics}},
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
#' # Strict three-level decomposition: occasions within people within dyads.
#' nested_data <- data.frame(
#'   person = rep(seq_len(12), each = 3),
#'   dyad = rep(seq_len(6), each = 6),
#'   x = rep(c(-2, 0, 2), 12) + rep(seq_len(12), each = 3),
#'   y = rep(c(1, -2, 1), 12) +
#'     rep(rep(c(-1, 1), 6), each = 3) +
#'     rep(c(2, 1, 3, 6, 4, 5), each = 6)
#' )
#' nested_correlations <- wbCorr(
#'   nested_data,
#'   cluster = list(person = "person", dyad = "dyad"),
#'   inference = "none"
#' )
#' get_matrix(nested_correlations, "l2")
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
                   centering_rows = c("pairwise_complete", "all_available"),
                   missing_data = c("pairwise", "listwise")) {

  inference_missing <- missing(inference)
  between_weighting_missing <- missing(between_weighting)
  between_inference_missing <- missing(between_inference)
  centering_rows_missing <- missing(centering_rows)
  missing_data_missing <- missing(missing_data)

  if (is.list(cluster)) {
    return(wbCorr_three_level(
      data = data,
      cluster = cluster,
      confidence_level = confidence_level,
      method = method,
      bootstrap = bootstrap,
      nboot = nboot,
      inference = inference,
      weighted_between_statistics = weighted_between_statistics,
      between_weighting = between_weighting,
      between_inference = between_inference,
      centering_rows = centering_rows,
      missing_data = missing_data,
      inference_missing = inference_missing,
      between_weighting_missing = between_weighting_missing,
      between_inference_missing = between_inference_missing,
      centering_rows_missing = centering_rows_missing,
      missing_data_missing = missing_data_missing,
      supplied_call = match.call()
    ))
  }

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
  missing_data <- resolve_wbcorr_choice(
    missing_data,
    c("pairwise", "listwise"),
    "missing_data",
    missing_data_missing
  )

  if (missing_data == "listwise" && centering_rows == "all_available") {
    warning(paste0(
      "centering_rows = 'all_available' is equivalent to ",
      "'pairwise_complete' after listwise deletion; using ",
      "'pairwise_complete'."
    ), call. = FALSE)
    centering_rows <- "pairwise_complete"
  }

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

  # Split variance into between- and within
  centered_df <- wbCenter(input_data, cluster_var, method,
                          cluster_size_between)
  icc_input_data <- centered_df$input_data_cleaned
  icc_cluster_var <- cluster_var
  validated_var_type <- centered_df$var_type
  validated_warnings <- centered_df$warnings

  if (missing_data == "listwise") {
    cleaned_data <- centered_df$input_data_cleaned
    complete_rows <- complete.cases(cleaned_data, cluster_var)
    cleaned_data <- cleaned_data[complete_rows, , drop = FALSE]
    cluster_var <- droplevels(as.factor(cluster_var[complete_rows]))

    if (nrow(cleaned_data) > 0L) {
      centered_df <- wbCenter(cleaned_data,
                              cluster_var,
                              method,
                              cluster_size_between)
    } else {
      # Preserve the validated variables and warnings while allowing the normal
      # pair-retention machinery to report every coefficient as unavailable.
      empty_level_data <- data.frame(cluster = cluster_var,
                                     cleaned_data,
                                     check.names = FALSE)
      centered_df$within <- empty_level_data
      centered_df$between <- empty_level_data
      centered_df$input_data_cleaned <- cleaned_data
    }
    # Coercion and assumption diagnostics describe the validated inputs, not
    # only the subset that happened to survive global deletion.
    centered_df$var_type <- validated_var_type
    centered_df$warnings <- validated_warnings
  }

  within_df <- centered_df$within[-1]
  between_df <- centered_df$between[-1]
  input_data_cleaned <- centered_df$input_data_cleaned
  var_type <- centered_df$var_type
  warnings <- centered_df$warnings

  centered_data <- list(within_df = within_df,
                        between_df = between_df,
                        analysis_data = input_data_cleaned,
                        cluster_var = cluster_var)

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

  within_methods <- unique(within_table$method[within_table$status == "ok"])
  between_methods <- unique(between_table$method[between_table$status == "ok"])
  within_matrix_diagnostics <- correlation_matrix_diagnostics(
    within_corr_coefs,
    level = "within",
    missing_data = missing_data,
    guaranteed_by_construction = missing_data == "listwise" &&
      length(within_methods) <= 1L
  )
  between_matrix_diagnostics <- correlation_matrix_diagnostics(
    between_corr_coefs,
    level = "between",
    missing_data = missing_data,
    guaranteed_by_construction = missing_data == "listwise" &&
      length(between_methods) <= 1L
  )
  warn_non_psd_matrix(within_matrix_diagnostics)
  warn_non_psd_matrix(between_matrix_diagnostics)


  # Calculate ICCs
  ICC <- compute_icc1(icc_input_data, icc_cluster_var)


  # Store everything in three sections of the object
  within <- list(correlations = within_corr_coefs,
                 p_values = within_p_values,
                 confidence_intervals = within_confidence_intervals,
                 table = within_table,
                 matrix_diagnostics = within_matrix_diagnostics)
  between <- list(correlations = between_corr_coefs,
                  p_values = between_p_values,
                  confidence_intervals = between_confidence_intervals,
                  table = between_table,
                  matrix_diagnostics = between_matrix_diagnostics)

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
                   missing_data = missing_data,
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

#' @rdname wbCorr-class
#' @description `wbCorrNested` extends `wbCorr` with separate result sections
#' for a strictly nested three-level decomposition.
#' @slot levels A named list containing `level1`, `level2`, and `level3`
#' correlation results.
#' @export
methods::setClass(
  "wbCorrNested",
  contains = "wbCorr",
  slots = c(levels = "list")
)

#' @rdname wbCorr
#' @export
wbcorr <- wbCorr

#######################################################
# Print()
#######################################################

# Set method for printing
#' @title Print a wbCorr result
#' @description Prints a compact summary of a two-level `wbCorr` or
#' three-level `wbCorrNested` object.
#' @param x A `wbCorr` or `wbCorrNested` object.
#' @param ... Additional arguments, currently unused.
#' @return Invisibly returns the supplied object. Ordinary `wbCorr` objects
#' print the within-cluster, between-cluster, and ICC tables; `wbCorrNested`
#' objects print separate level-1, level-2, and level-3 tables.
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

  cat("\nInspect matrices with summary(object, which = c('w', 'b', 'wb'))")
  cat("\nAccess full tables with get_tables(object, which = c('within', 'between'))")
  cat("\nAccess matrices programmatically with get_matrix(object, numeric = TRUE)")
  cat("\nAccess the full ICC table with get_ICC(object)\n")
  invisible(x)
})

#' @rdname print.wbCorr
#' @export
methods::setMethod("print", signature("wbCorrNested"), function(x, ...) {
  cat("\n---- wbCorr Three-Level Object ----\n")
  cat("Call: ", deparse(x@call), "\n")

  level_labels <- x@settings$level_labels
  for (level in c("level1", "level2", "level3")) {
    title <- sprintf("%s correlations:", level_labels[[level]])
    cat("\n", title, "\n", sep = "")
    cat(strrep("-", nchar(title)), "\n")
    table <- x@levels[[level]]$table
    print(utils::head(table))
    if (nrow(table) > 6L) {
      cat("... ", nrow(table) - 6L, " more rows\n", sep = "")
    }
  }

  cat("\nInspect matrices with summary(object, which = c('l1', 'l2', 'l3'))")
  cat("\nAccess full tables with get_table(object)")
  cat("\nA single three-level ICC and merged three-level matrix are not defined.\n")
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

#' @title Plot level-specific associations
#' @description Plots the level-specific variables against each other. For a
#' two-level object, choose cluster means (`"between"`) or deviations from
#' cluster means (`"within"`). For a three-level object, choose `"level1"`/
#' `"l1"`, `"level2"`/`"l2"`, or `"level3"`/`"l3"`. Every panel uses the same
#' pair-specific rows, decomposition, method, and weights as the fitted object.
#' Pearson plots draw a corresponding regression line and annotate the stored
#' correlation; weighted between-cluster panels use weighted least squares.
#' Spearman plots report the stored rho without a linear-regression overlay.
#' Significance stars are shown only when the fitted wbCorr object contains a
#' p-value for that pair.
#' @param x A `wbCorr` or `wbCorrNested` object to be plotted.
#' @param y Choose which correlations to plot. Use `"within"`/`"w"` or
#' `"between"`/`"b"` for a two-level object, and `"level1"`/`"l1"`,
#' `"level2"`/`"l2"`, or `"level3"`/`"l3"` for a three-level object. This can
#' be supplied positionally.
#' @param which An alternative to `y` that takes precedence when both are
#' supplied.
#' @param plot_NA Boolean. Whether variables that have no variation on the selected level should be plotted or not.
#' @param standardize Boolean. Whether each plotted pair should be standardized
#' using the same weights as its fitted coefficient. For Pearson panels, this
#' makes the displayed regression slope equal to the stored correlation.
#' @param outlier_detection If FALSE, outliers will not be marked in red. Otherwise you may provide the method. Choose from: 'zscore', 'mad', or 'tukey'.
#' @param outlier_threshold If 'recommended', the threshold for 'zscore' and 'mad' will be set to 3, and for 'tukey' to 1.5. You can provide and other numeric here.
#' @param type points, lines, etc. see ?base::plot for available types).
#' @param pch Graphical parameter. Select which type of points should be plotted.
#' @param dot_lwd Graphical parameter. Set size of the points.
#' @param reg_lwd Graphical parameter. Set thickness of the regression line.
#' @param ... further options to be passed to the base plot (pairs) function.
#' @return Invisibly returns the supplied object. Called for the side effect of
#' drawing a pairs plot of the selected level-specific variables.
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

#' @rdname plot-wbCorr-method
#' @export
methods::setMethod(
  "plot",
  signature(x = "wbCorrNested", y = "ANY"),
  wb_plot_nested
)
