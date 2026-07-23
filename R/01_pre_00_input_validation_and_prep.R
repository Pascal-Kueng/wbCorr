

validate_wbcorr_inputs <- function(input_data,
                                   cluster,
                                   confidence_level,
                                   method,
                                   bootstrap,
                                   nboot,
                                   weighted_between_statistics) {
  if (!is.data.frame(input_data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (nrow(input_data) < 1L) {
    stop("data must contain at least one row.", call. = FALSE)
  }

  if (!is.numeric(confidence_level) ||
      length(confidence_level) != 1L ||
      is.na(confidence_level) ||
      !is.finite(confidence_level) ||
      confidence_level <= 0 ||
      confidence_level >= 1) {
    stop("confidence_level must be one finite numeric value strictly between 0 and 1.",
         call. = FALSE)
  }

  method_choices <- c("pearson", "spearman", "spearman-jackknife", "auto")
  if (!is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !method %in% method_choices) {
    stop("method must be exactly one of: 'pearson', 'spearman', 'spearman-jackknife', or 'auto'.",
         call. = FALSE)
  }

  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap)) {
    stop("bootstrap must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.null(weighted_between_statistics) &&
      (!is.logical(weighted_between_statistics) ||
       length(weighted_between_statistics) != 1L ||
       is.na(weighted_between_statistics))) {
    stop("weighted_between_statistics must be NULL or one non-missing logical value.",
         call. = FALSE)
  }

  if (!is.numeric(nboot) ||
      length(nboot) != 1L ||
      is.na(nboot) ||
      !is.finite(nboot) ||
      nboot < 10 ||
      nboot > .Machine$integer.max ||
      nboot != floor(nboot)) {
    stop("nboot must be one finite whole number of at least 10.", call. = FALSE)
  }

  if (is.null(cluster) || length(cluster) == 0L) {
    stop("cluster must identify a data column or supply one value per data row.",
         call. = FALSE)
  }

  named_cluster <- is.character(cluster) &&
    length(cluster) == 1L &&
    !is.na(cluster)

  if (named_cluster) {
    if (!cluster %in% colnames(input_data)) {
      stop("cluster must name a column in data or supply one value per data row.",
           call. = FALSE)
    }
    cluster_values <- input_data[[cluster]]
  } else {
    cluster_values <- cluster
  }

  if (!is.atomic(cluster_values) || !is.null(dim(cluster_values)) ||
      length(cluster_values) != nrow(input_data)) {
    stop("The cluster variable must be atomic, dimensionless, and have exactly one value per data row.",
         call. = FALSE)
  }
  if (all(is.na(cluster_values))) {
    stop("The cluster variable must contain at least one non-missing identifier.",
         call. = FALSE)
  }
  if (is.numeric(cluster_values) &&
      any(is.infinite(cluster_values) | is.nan(cluster_values))) {
    stop("A numeric cluster variable may contain NA but not Inf, -Inf, or NaN.",
         call. = FALSE)
  }

  invisible(TRUE)
}


resolve_wbcorr_choice <- function(value, choices, argument, was_missing) {
  if (was_missing) {
    return(choices[[1L]])
  }
  if (!is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !value %in% choices) {
    stop(sprintf("%s must be exactly one of: %s.",
                 argument,
                 paste(sprintf("'%s'", choices), collapse = ", ")),
         call. = FALSE)
  }
  value
}


input_validation_and_prep <- function(input_data, cluster, method, weighted_between_statistics, bootstrap) {
  # Input validation and error handling

  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (!method %in% c("pearson", "spearman", "spearman-jackknife", "auto")) {
    stop("Invalid correlation method. Choose one of: 'pearson', 'spearman', and 'spearman-jackknife'.")
  }
  if (method == 'spearman-jackknife' & isTRUE(weighted_between_statistics)) {
    stop("cluster-size weighted between statistics are not supported for jackknife CIs.")
  }
  if (method == 'spearman-jackknife' & bootstrap == TRUE) {
    stop("Jackknife and bootstraping can't both be active at once.")
  }

  if (is.character(cluster) && length(cluster) == 1L) {
    return(as.factor(input_data[[cluster]]))
  }

  as.factor(cluster)
}

remove_cluster_columns <- function(input_data, cluster) {
  if (length(cluster) == 1 && is.character(cluster) && cluster %in% colnames(input_data)) {
    input_data[[cluster]] <- NULL
    return(input_data)
  }

  # When the values supplied as `cluster` are also present in data, remove that
  # column even if its storage type differs (for example, integer vs double).
  # Compare displayed values and the NA pattern, not factor codes: unrelated
  # outcomes with the same code pattern must remain available for analysis.
  for (name in colnames(input_data)) {
    column <- input_data[[name]]
    same_missingness <- length(column) == length(cluster) &&
      identical(is.na(column), is.na(cluster))
    same_observed_values <- FALSE
    if (same_missingness) {
      observed_column <- column[!is.na(column)]
      observed_cluster <- cluster[!is.na(cluster)]
      if (is.numeric(observed_column) && is.numeric(observed_cluster)) {
        same_observed_values <- identical(as.numeric(observed_column),
                                          as.numeric(observed_cluster))
      } else if (xor(is.numeric(observed_column),
                     is.numeric(observed_cluster))) {
        numeric_values <- if (is.numeric(observed_column)) {
          as.numeric(observed_column)
        } else {
          as.numeric(observed_cluster)
        }
        text_values <- if (is.numeric(observed_column)) {
          as.character(observed_cluster)
        } else {
          as.character(observed_column)
        }
        parsed_values <- suppressWarnings(as.numeric(text_values))
        same_observed_values <- !anyNA(parsed_values) &&
          identical(numeric_values, parsed_values)
      } else {
        same_observed_values <- identical(as.character(observed_column),
                                          as.character(observed_cluster))
      }
    }
    if (same_observed_values) {
      input_data[[name]] <- NULL
    }
  }

  input_data
}
