correlation_matrix_diagnostics <- function(correlation_matrix,
                                           level,
                                           missing_data,
                                           guaranteed_by_construction = FALSE) {
  matrix_values <- as.matrix(correlation_matrix)
  n_variables <- nrow(matrix_values)

  diagnostic <- data.frame(
    level = level,
    status = "not_assessable",
    is_complete = FALSE,
    is_psd = NA,
    min_eigenvalue = NA_real_,
    tolerance = NA_real_,
    n_variables = as.integer(n_variables),
    missing_data = missing_data,
    guaranteed_by_construction = FALSE,
    reason = "matrix_contains_missing_or_non_finite_entries",
    stringsAsFactors = FALSE
  )

  if (n_variables == 0L || ncol(matrix_values) != n_variables ||
      any(!is.finite(matrix_values))) {
    return(diagnostic)
  }

  # Symmetrize before the eigendecomposition to remove harmless floating-point
  # asymmetry. The tolerance distinguishes numerical noise from a substantively
  # negative eigenvalue without repairing or otherwise changing the matrix.
  symmetric_matrix <- (matrix_values + t(matrix_values)) / 2
  eigenvalues <- eigen(symmetric_matrix,
                       symmetric = TRUE,
                       only.values = TRUE)$values
  scale <- max(1, max(abs(eigenvalues)))
  tolerance <- sqrt(.Machine$double.eps) * scale
  min_eigenvalue <- min(eigenvalues)
  is_psd <- min_eigenvalue >= -tolerance

  diagnostic$status <- if (is_psd) {
    "positive_semidefinite"
  } else {
    "non_positive_semidefinite"
  }
  diagnostic$is_complete <- TRUE
  diagnostic$is_psd <- is_psd
  diagnostic$min_eigenvalue <- min_eigenvalue
  diagnostic$tolerance <- tolerance
  diagnostic$guaranteed_by_construction <-
    isTRUE(guaranteed_by_construction)
  diagnostic$reason <- NA_character_
  diagnostic
}


warn_non_psd_matrix <- function(diagnostic) {
  if (!identical(diagnostic$is_psd[[1L]], FALSE)) {
    return(invisible(NULL))
  }

  level_label <- if (diagnostic$level[[1L]] == "within") {
    "within-cluster"
  } else {
    "between-cluster"
  }
  remedy <- if (diagnostic$missing_data[[1L]] == "pairwise") {
    paste0(
      "Pair-specific missing-data samples can cause this; use ",
      "missing_data = 'listwise' when a common-row matrix is required."
    )
  } else {
    paste0(
      "Listwise deletion uses common rows, but mixed coefficient types ",
      "(method = 'auto') can still prevent a common correlation representation."
    )
  }

  warning(
    sprintf(
      paste0(
        "The %s correlation matrix is not positive semidefinite ",
        "(minimum eigenvalue %.6g). %s See get_matrix_diagnostics()."
      ),
      level_label,
      diagnostic$min_eigenvalue[[1L]],
      remedy
    ),
    call. = FALSE
  )
  invisible(NULL)
}


#' Inspect positive-semidefinite diagnostics for correlation matrices
#'
#' @description Returns matrix-level diagnostics computed from the unrounded
#' within- and between-cluster correlation matrices stored in a [wbCorr()]
#' object. Pairwise-complete correlations can be non-positive-semidefinite when
#' different pairs use different rows. A diagnostic is not assessable when the
#' corresponding matrix contains an unavailable (`NA`) coefficient.
#'
#' @param object A `wbCorr` object.
#'
#' @return A data frame with one row for each level and columns identifying the
#' diagnostic status, positive-semidefinite result, minimum eigenvalue,
#' completeness, numerical tolerance, number of variables, missing-data mode,
#' whether PSD followed from a common-matrix construction, and reason when
#' assessment was unavailable.
#'
#' @seealso [wbCorr()], [get_matrix()]
#' @export
get_matrix_diagnostics <- function(object) {
  if (!methods::is(object, "wbCorr")) {
    stop("object must be a wbCorr object.", call. = FALSE)
  }

  extract_diagnostic <- function(section, level) {
    diagnostic <- section$matrix_diagnostics
    if (!is.null(diagnostic)) {
      return(diagnostic)
    }

    data.frame(
      level = level,
      status = "not_available",
      is_complete = FALSE,
      is_psd = NA,
      min_eigenvalue = NA_real_,
      tolerance = NA_real_,
      n_variables = as.integer(nrow(section$correlations)),
      missing_data = NA_character_,
      guaranteed_by_construction = FALSE,
      reason = "object_created_without_matrix_diagnostics",
      stringsAsFactors = FALSE
    )
  }

  rbind(
    extract_diagnostic(object@within, "within"),
    extract_diagnostic(object@between, "between")
  )
}
