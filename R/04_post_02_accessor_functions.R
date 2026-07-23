################################
# get_table / get_tables
################################

#' @title Retrieve full tables for both within- and/or between-cluster correlations for a wbCorr object.
#' @description This function has an alias get_tables() which can be used interchangeably.
#' For correlations matrices, see the summary() function.
#'
#' @param object A wbCorr object, created by the wbCorr() function.
#' @param which A character vector indicating which correlation table to return.
#' Options are 'within' or 'w', and 'between' or 'b'.
#'
#' @return A list containing the selected detailed tables of within- and/or
#' between-cluster correlations. Each table retains every requested pair and
#' includes raw pair-row, contributing-cluster, bootstrap-yield, coefficient-
#' status, and inference-status diagnostics; see [wbCorr()] for definitions.
#'
#' @seealso \code{\link[=summary.wbCorr]{summary}}, \code{\link[=wbCorr]{wbCorr}}
#' @examples
#' # importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#'
#' # create object:
#' correlations <- wbCorr(data = simdat_intensive_longitudinal,
#'                       cluster = 'participantID')
#'
#' # returns a list with full detailed tables of the correlations:
#' tables <- get_table(correlations) # the get_tables() function is equivalent
#' print(tables)
#'
#' # Access specific tables by:
#' # Option 1:
#' tables$between
#' # Option 2:
#' within_table <- get_tables(correlations, which = 'w') # or use 'within' or 'between'
#' print(within_table) # within_table could be saved to an excel or csv file (e.g., write.csv)
#'
#' @export
get_table <- function(object, which = c('within', 'between')) {
  which <- match.arg(which, choices = c('within', 'w', 'between', 'b'),
                     several.ok = TRUE) # Check for valid inputs
  output_list <- list()
  if ('within' %in% which | 'w' %in% which) {
    output_list[['within']] <- object@within$table
  }
  if ('between' %in% which | 'b' %in% which) {
    output_list[['between']] <- object@between$table
  }
  return(output_list)
}

# Alias get_table to get_tables
#' @rdname get_table
#' @export
get_tables <- get_table

##############################################
# Summary / get_matrix / get_matrices
##############################################

#' @title Return matrices for within- and/or between-cluster correlations.
#' @description You can use summary(), get_matrices(), or get_matrix() interchangeably.
#' Merged matrices include the ICC on the diagonal.
#' For more detailed statistics, use get_table(). By default, matrices are
#' presentation-formatted with two decimal places and significance stars. Set
#' `numeric = TRUE` to retrieve the stored, unrounded numeric coefficients.
#'
#' @param object A wbCorr object, created by the wbCorr() function.
#' @param which A string or a character vector indicating which summaries to return.
#' Options are 'within' or 'w', 'between' or 'b', and various merge options
#' like 'merge', 'm', 'merge_wb', 'wb', 'merge_bw', 'bw'.
#' Default is c('within', 'between', 'merge').
#' @param numeric A non-missing logical value. If `FALSE` (the default), return
#' presentation-formatted character matrices with two decimal places and any
#' available significance stars. If `TRUE`, return unrounded numeric
#' correlation matrices. Numeric merged matrices contain unrounded ICCs on the
#' diagonal.
#' @param ... Additional arguments passed to the base summary method
#'
#' @return A list containing the selected matrices of within- and/or
#' between-cluster correlations, and ICCs on the diagonals for merged matrices.
#' With `numeric = FALSE`, matrix entries are presentation-formatted character
#' values. With `numeric = TRUE`, matrix columns are numeric and retain the
#' full stored precision.
#' @seealso \code{\link[=get_table]{get_tables}}, \code{\link[=wbCorr]{wbCorr}}
#' @examples
#' # importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#'
#' # create object:
#' correlations <- wbCorr(data = simdat_intensive_longitudinal,
#'                       cluster = 'participantID')
#'
#' # returns a correlation matrix with stars for p-values:
#' matrices <- summary(correlations) # the get_matrix() and get_matrices() functions are equivalent
#' print(matrices)
#'
#' # Access specific matrices by:
#' # Option 1:
#' matrices$within
#' # Option 2:
#' within_matrix <- summary(correlations, which = 'w') # or use 'within'
#' merged_within_between <- summary(correlations, which = 'wb')
#' print(within_matrix) # could be saved to an excel or csv file (e.g., write.csv)
#'
#' # Retrieve unrounded numeric coefficients for downstream calculations:
#' numeric_matrices <- get_matrix(correlations, numeric = TRUE)
#' numeric_matrices$within
#'
#' @export
get_matrix <- function(object, which = c('within', 'between', 'merge'),
                       numeric = FALSE, ...) {
  if (!is.logical(numeric) || length(numeric) != 1L || is.na(numeric)) {
    stop("numeric must be one non-missing logical value.", call. = FALSE)
  }

  which <- match.arg(which, choices = c('within', 'w',
                                        'between', 'b',
                                        'merge','m', 'merged',
                                        'merge_bw', 'bw',
                                        'merge_wb', 'wb'), several.ok = TRUE) # Check for valid inputs

  if (numeric) {
    df_summary_within <- numeric_correlation_data_frame(
      object@within$correlations
    )
    df_summary_between <- numeric_correlation_data_frame(
      object@between$correlations
    )
    combined_df_wb <- combine_numeric_matrices(
      df_summary_within,
      df_summary_between,
      object@ICC
    )
    combined_df_bw <- combine_numeric_matrices(
      df_summary_between,
      df_summary_within,
      object@ICC
    )
  } else {
    df_summary_within <- summarize_table(
      object@within$p_values,
      object@within$correlations)

    df_summary_between <- summarize_table(
      object@between$p_values,
      object@between$correlations)

    combined_df_wb <- combine_matrices(as.matrix(df_summary_within),
                                       as.matrix(df_summary_between),
                                       object@ICC)
    combined_df_bw <- combine_matrices(as.matrix(df_summary_between),
                                       as.matrix(df_summary_within),
                                       object@ICC)
  }

  return_list <- list()
  if ('within' %in% which | 'w' %in% which) {
    return_list[['within']] <- df_summary_within
  }
  if ('between' %in% which | 'b' %in% which) {
    return_list[['between']] <- df_summary_between
  }
  if ('merge' %in% which | 'm' %in% which | 'merged' %in% which) {
    return_list[['merged_wb']] <- combined_df_wb
    return_list[['note_wb']] <- "Top-right triangle: Within-correlations. Bottom-left triangle: Between-correlations. Diagonal: ICC"
    return_list[['merged_bw']] <- combined_df_bw
    return_list[['note_bw']] <- "Top-right triangle: Between-correlations. Bottom-left triangle: Within-correlations. Diagonal: ICC"

  } else if ('merge_bw' %in% which | 'bw' %in% which) {
    return_list[['merged_bw']] <- combined_df_bw
  } else if ('merge_wb' %in% which | 'wb' %in% which) {
    return_list[['merged_wb']] <- combined_df_wb
  }
  has_off_diagonal_p <- function(p_values) {
    p_matrix <- as.matrix(p_values)
    diag(p_matrix) <- NA_real_
    any(is.finite(p_matrix))
  }
  if (!numeric &&
      (has_off_diagonal_p(object@within$p_values) ||
       has_off_diagonal_p(object@between$p_values))) {
    return_list[['note']] <- '***p < 0.001, **p < 0.01, *p < 0.05'
  }
  return(return_list)
}


numeric_correlation_data_frame <- function(correlations) {
  correlation_matrix <- as.matrix(correlations)
  storage.mode(correlation_matrix) <- "double"
  as.data.frame(correlation_matrix, check.names = FALSE)
}


combine_numeric_matrices <- function(upper_matrix, lower_matrix, ICC) {
  combined_matrix <- as.matrix(upper_matrix)
  lower_values <- as.matrix(lower_matrix)
  combined_matrix[lower.tri(combined_matrix)] <-
    lower_values[lower.tri(lower_values)]

  icc_match <- match(rownames(combined_matrix), ICC$variable)
  diagonal_values <- diag(combined_matrix)
  has_icc <- !is.na(icc_match)
  diagonal_values[has_icc] <- ICC$ICC[icc_match[has_icc]]
  diag(combined_matrix) <- diagonal_values

  storage.mode(combined_matrix) <- "double"
  as.data.frame(combined_matrix, check.names = FALSE)
}


#' @rdname  get_matrix
#' @export
get_matrices <- get_matrix


##############################################
# get_ICC(), get_ICCs()
##############################################

#' @title Return all ICCs for the original variables.
#' @description You can use get_ICC() or get_ICCs() interchangeably.
#'
#' @param object A wbCorr object, created by the wbCorr() function.
#' @return A data frame with the one-way random-effects, single-measure
#' ICC(1,1) for every variable. Each ICC is estimated separately from all finite
#' observations with a non-missing cluster identifier. The ANOVA
#' method-of-moments estimator uses an effective cluster size for unbalanced
#' clusters. Negative sample estimates are retained and can be less than -1 in
#' severely unbalanced samples. The population interpretation assumes
#' independent clusters, a common within-cluster variance, and noninformative
#' cluster size and missingness. `NA` is returned when an ICC cannot be estimated
#' because there are fewer than two clusters, no within-cluster replication, or
#' zero total variability.
#' @references Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations:
#' Uses in assessing rater reliability. *Psychological Bulletin, 86*(2),
#' 420-428. \doi{10.1037/0033-2909.86.2.420}
#'
#' Ohyama, T. (2025). A comparison of confidence interval methods for the
#' intraclass correlation coefficient based on the one-way random effects
#' model. *Japanese Journal of Statistics and Data Science, 8*, 587-602.
#' \doi{10.1007/s42081-025-00292-3}
#'
#' Wang, C.-M., Yandell, B. S., & Rutledge, J. J. (1992). The dilemma of
#' negative analysis of variance estimators of intraclass correlation.
#' *Theoretical and Applied Genetics, 85*, 79-88.
#' \doi{10.1007/BF00223848}
#' @seealso \code{\link[=wbCorr]{wbCorr}}
#' @examples
#' # importing our simulated example dataset with pre-specified within- and between- correlations
#' data("simdat_intensive_longitudinal")
#'
#' # create object:
#' correlations <- wbCorr(data = simdat_intensive_longitudinal,
#'                       cluster = 'participantID')
#'
#' # returns the ICCs:
#' ICCs <- get_ICC(correlations)
#' print(ICCs)
#'
#' @export
get_ICC <- function(object) {
  return(object@ICC)
}

#' @rdname  get_ICC
#' @export
get_ICCs <- get_ICC

#' @rdname get_ICC
#' @export
get_icc <- get_ICC
