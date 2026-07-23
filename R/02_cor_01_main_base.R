
#######################################################
# Calculating Statistics
#######################################################
#' @importFrom stats pnorm qchisq qnorm uniroot var
# This function calculates the correlation coefficients, p-values, and confidence intervals for the input data.
corAndPValues <- function(input_data,
                          n_clusters_between = NULL,
                          confidence_level = 0.95,
                          method = "pearson",
                          auto_type,
                          var_type,
                          warnings,
                          bootstrap,
                          nboot,
                          cluster_var = NULL,
                          level = c('plain', 'within', 'between'),
                          between_weighting = c('equal_clusters', 'cluster_size'),
                          between_inference = c('analytic', 'none'),
                          weighted_analytic_requested = FALSE,
                          centering_rows = c('pairwise_complete', 'all_available'),
                          inference = c('analytic', 'none', 'cluster_bootstrap'),
                          requested_inference = inference) {
  level <- match.arg(level)
  between_weighting <- match.arg(between_weighting)
  between_inference <- match.arg(between_inference)
  centering_rows <- match.arg(centering_rows)
  inference <- match.arg(inference)

  # initializing matrices and lists
  value_list <- initializing_values(input_data)

  n_numeric <- value_list$n_numeric
  p_matrix <- value_list$p_matrix
  cor_matrix <- value_list$cor_matrix
  conf_int_df <- value_list$conf_int_df
  result_table <- value_list$result_table
  idx_combinations <- value_list$idx_combinations

  for (k in 1:nrow(idx_combinations)) {
    i <- idx_combinations[k, 1]
    j <- idx_combinations[k, 2]
    parameter_i <- names(input_data)[i]
    parameter_j <- names(input_data)[j]

    pair_data <- prepare_correlation_pair(input_data[[i]],
                                          input_data[[j]],
                                          cluster_var,
                                          level,
                                          between_weighting,
                                          centering_rows)
    col_i <- pair_data$col_i
    col_j <- pair_data$col_j
    weights <- pair_data$weights
    n_comparisons <- pair_data$n_comparisons
    k_pair <- pair_data$k_pair
    n_obs <- pair_data$n_obs
    n_clusters <- pair_data$n_clusters

    # set method
    method_selected <- method
    if (auto_type) {
      type_i <- var_type[[names(input_data)[i]]]
      type_j <- var_type[[names(input_data)[j]]]

      method_selected <- 'pearson'
      method_table <- "pearson's r"


      if (type_i == 'ordinal' | type_j == 'ordinal') {
        method_selected <- 'spearman'
        method_table <- if (level == 'within') {
          "centered-score Spearman rho"
        } else {
          "cluster-mean Spearman rho"
        }
      } else {
        if(type_i == 'binary' | type_j == 'binary') {
          method_selected <- 'pearson'
          method_table <- "pearson's r"
        }
      }
    } else if (method_selected == 'pearson') {
      method_table <- "pearson's r"
    } else if (method_selected %in% c('spearman', 'spearman-jackknife')) {
      method_table <- "spearman's rho"
    }

    warning_i <- warnings[[parameter_i]]
    warning_j <- warnings[[parameter_j]]

    auto_warning <- 'None'
    if (!warning_i == 'None') {
      auto_warning <- append_cor_warning(auto_warning, warning_i)
    }
    if (!warning_j == 'None') {
      auto_warning <- append_cor_warning(auto_warning, warning_j)
    }

    pair_state <- correlation_pair_state(col_i, col_j, level)
    status <- pair_state$status
    reason <- pair_state$reason
    degrees_freedom <- NA_real_
    statistic_type <- NA_character_

    if (status == 'ok' && method_selected == 'pearson') {
      degrees_freedom <- correlation_degrees_freedom(method_selected,
                                                     level,
                                                     n_comparisons,
                                                     k_pair,
                                                     n_clusters_between)
      statistic_type <- 't-statistic'
    } else if (status == 'ok' && method_selected == 'spearman') {
      degrees_freedom <- correlation_degrees_freedom(method_selected,
                                                     level,
                                                     n_comparisons,
                                                     k_pair,
                                                     n_clusters_between)
      statistic_type <- 'z-statistic'
    }

    weighted_between_analytic <- level == 'between' &&
      between_weighting == 'cluster_size' &&
      inference == 'analytic' &&
      (between_inference == 'analytic' || weighted_analytic_requested)

    if (status != 'ok') {
      correlations_statistics_list <- list(
        correlation_coefficient = NA_real_,
        test_statistic = NA_real_,
        p_value = NA_real_,
        lower_bound = NA_real_,
        upper_bound = NA_real_,
        n_boot_attempted = if (inference == 'cluster_bootstrap') 0L else NA_integer_,
        n_boot_valid = if (inference == 'cluster_bootstrap') 0L else NA_integer_
      )
    } else if (inference == 'none' ||
        (inference == 'analytic' &&
         level == 'between' &&
         between_inference == 'none') ||
        weighted_between_analytic) {
      cor_method <- if (method_selected == 'spearman-jackknife') {
        'spearman'
      } else {
        method_selected
      }
      if (is.null(weights)) {
        correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                        method = cor_method))
      } else {
        correlation_coefficient <- weighted_cor(col_i, col_j, weights, cor_method)
      }
      correlations_statistics_list <- list(correlation_coefficient = correlation_coefficient,
                                           test_statistic = NA_real_,
                                           p_value = NA_real_,
                                           lower_bound = NA_real_,
                                           upper_bound = NA_real_,
                                           n_boot_attempted = NA_integer_,
                                           n_boot_valid = NA_integer_)
      statistic_type <- NA_character_
      degrees_freedom <- NA_real_
    } else if (inference == 'cluster_bootstrap') {
      observed_correlation <- correlation_from_pair(pair_data, method_selected)
      if (!is.na(n_clusters) && n_clusters < 3L) {
        correlations_statistics_list <- list(
          correlation_coefficient = observed_correlation,
          test_statistic = NA_real_,
          p_value = NA_real_,
          lower_bound = NA_real_,
          upper_bound = NA_real_,
          n_boot_attempted = 0L,
          n_boot_valid = 0L
        )
      } else {
        correlations_statistics_list <- cluster_bootstrap_statistics(
          input_data = input_data,
          column_i = i,
          column_j = j,
          cluster_var = cluster_var,
          level = level,
          between_weighting = between_weighting,
          centering_rows = centering_rows,
          method = method_selected,
          confidence_level = confidence_level,
          nboot = nboot,
          observed_correlation = observed_correlation
        )
      }
      statistic_type <- NA
      degrees_freedom <- NA
    } else {
      correlations_statistics_list <- calculate_correlations_and_statistics(col_i, col_j,
                                                                            method_selected,
                                                                            degrees_freedom,
                                                                            confidence_level,
                                                                            FALSE,
                                                                            nboot,
                                                                            weights = weights)
    }

    correlation_coefficient <- correlations_statistics_list$correlation_coefficient
    test_statistic <- correlations_statistics_list$test_statistic
    p_value <- correlations_statistics_list$p_value
    lower_bound <- correlations_statistics_list$lower_bound
    upper_bound <- correlations_statistics_list$upper_bound
    n_boot_attempted <- correlations_statistics_list$n_boot_attempted
    if (is.null(n_boot_attempted)) {
      n_boot_attempted <- NA_integer_
    }
    n_boot_valid <- correlations_statistics_list$n_boot_valid
    if (is.null(n_boot_valid)) {
      n_boot_valid <- NA_integer_
    }

    if (status == 'ok' &&
        (length(correlation_coefficient) != 1L ||
         is.na(correlation_coefficient) ||
         !is.finite(correlation_coefficient))) {
      status <- 'not_estimable'
      reason <- 'non_finite_correlation'
      correlation_coefficient <- NA_real_
      test_statistic <- p_value <- lower_bound <- upper_bound <- NA_real_
    }

    inference_state <- correlation_inference_state(
      status = status,
      requested_inference = requested_inference,
      level = level,
      between_inference = between_inference,
      method_selected = method_selected,
      weighted_between_analytic = weighted_between_analytic,
      degrees_freedom = degrees_freedom,
      p_value = p_value,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      n_boot_valid = n_boot_valid,
      n_clusters = n_clusters,
      nboot = nboot
    )

    if (weighted_between_analytic) {
      auto_warning <- append_cor_warning(auto_warning,
                                         'weighted between analytic inference unavailable; coefficient only')
    }

    if (requested_inference == 'cluster_bootstrap' &&
        !is.na(n_boot_attempted) &&
        n_boot_attempted > 0L &&
        n_boot_valid < n_boot_attempted) {
      auto_warning <- append_cor_warning(
        auto_warning,
        sprintf('%d of %d cluster-bootstrap replicates produced finite coefficients',
                n_boot_valid,
                n_boot_attempted)
      )
    }

    if (centering_rows == 'all_available' &&
        level %in% c('within', 'between') &&
        inference == 'analytic' &&
        method_selected != 'spearman-jackknife') {
      auto_warning <- append_cor_warning(auto_warning,
                                         'all-available centering uses variable-specific mean rows; analytic inference approximate')
    }

    # populate matrices
    cor_matrix[i, j] <- cor_matrix[j, i] <- correlation_coefficient
    p_matrix[i, j] <- p_matrix[j, i] <- p_value

    # populate CI interval df
    temp_ci_df <- data.frame(Parameter1 = parameter_i,
                             Parameter2 = parameter_j,
                             CI_lower = lower_bound,
                             correlation_coefficient = correlation_coefficient,
                             CI_upper = upper_bound)
    # Make sure the column names of temp_ci_df match those of conf_int_df
    colnames(temp_ci_df) <- colnames(conf_int_df)
    conf_int_df <- rbind(conf_int_df, temp_ci_df)

    ci_text <- if (is.na(lower_bound) && is.na(upper_bound)) {
      NA_character_
    } else {
      sprintf('[%0.2f, %0.2f]', lower_bound, upper_bound)
    }

    # populate big dataframe
    temp_df <- data.frame(Parameter1 = parameter_i,
                          Parameter2 = parameter_j,
                          warning = auto_warning,
                          method = method_table,
                          coefficient = round(correlation_coefficient, 2),
                          statistic_type = statistic_type,
                          statistic = round(test_statistic, 2),
                          df = degrees_freedom,
                          CI = ci_text,
                          p = p_value,
                          n_obs = as.integer(n_obs),
                          n_clusters = as.integer(n_clusters),
                          n_boot_attempted = as.integer(n_boot_attempted),
                          n_boot_valid = as.integer(n_boot_valid),
                          status = status,
                          reason = reason,
                          inference_status = inference_state$status,
                          inference_reason = inference_state$reason)
    result_table <- rbind(result_table, temp_df)
  }

  # A diagonal is estimable only when that variable has positive variance at
  # the requested level. Use a self-pair so missing cluster identifiers and the
  # within/between decomposition are handled exactly as for off-diagonal pairs.
  diagonal_method <- if (method == 'spearman') 'spearman' else 'pearson'
  for (i in 1:n_numeric) {
    diagonal_pair <- prepare_correlation_pair(input_data[[i]],
                                              input_data[[i]],
                                              cluster_var,
                                              level,
                                              between_weighting,
                                              centering_rows)
    diagonal <- correlation_from_pair(diagonal_pair, diagonal_method)
    cor_matrix[i, i] <- if (length(diagonal) == 1L &&
                            is.finite(diagonal)) diagonal else NA_real_
  }
  diag(p_matrix) <- NA_real_

  # Converting the other matrices to DFs
  p_value_df <- as.data.frame(p_matrix)
  correlation_coefficient_df <- as.data.frame(cor_matrix)



  result_table <- format_result_table(result_table,
                                      method,
                                      auto_type,
                                      var_type,
                                      confidence_level,
                                      inference,
                                      level)

  return(list(p_value = p_value_df,
              correlation_coefficient = correlation_coefficient_df,
              confidence_intervals = conf_int_df,
              result_table = result_table))
}

prepare_correlation_pair <- function(col_i,
                                     col_j,
                                     cluster_var,
                                     level,
                                     between_weighting,
                                     centering_rows) {
  if (level == 'plain') {
    complete_cases <- complete.cases(col_i, col_j)
    return(list(col_i = col_i[complete_cases],
                col_j = col_j[complete_cases],
                weights = NULL,
                n_comparisons = sum(complete_cases),
                k_pair = NA_integer_,
                n_obs = sum(complete_cases),
                n_clusters = NA_integer_))
  }

  if (is.null(cluster_var)) {
    stop("cluster_var is required for within- and between-cluster correlations.")
  }

  complete_pair_cases <- complete.cases(col_i, col_j, cluster_var)
  col_i_pair <- col_i[complete_pair_cases]
  col_j_pair <- col_j[complete_pair_cases]
  cluster_pair <- droplevels(as.factor(cluster_var[complete_pair_cases]))
  n_comparisons <- length(col_i_pair)
  k_pair <- nlevels(cluster_pair)

  if (n_comparisons == 0 || k_pair == 0) {
    return(list(col_i = numeric(0),
                col_j = numeric(0),
                weights = NULL,
                n_comparisons = 0,
                k_pair = 0,
                n_obs = 0,
                n_clusters = 0))
  }

  if (level == 'within') {
    if (centering_rows == 'all_available') {
      mean_i <- cluster_means_for_pair_rows(col_i, cluster_var, cluster_pair)
      mean_j <- cluster_means_for_pair_rows(col_j, cluster_var, cluster_pair)
    } else {
      mean_i <- ave(col_i_pair, cluster_pair, FUN = mean)
      mean_j <- ave(col_j_pair, cluster_pair, FUN = mean)
    }

    return(list(col_i = col_i_pair - mean_i,
                col_j = col_j_pair - mean_j,
                weights = NULL,
                n_comparisons = n_comparisons,
                k_pair = k_pair,
                n_obs = n_comparisons,
                n_clusters = k_pair))
  }

  pair_df <- data.frame(cluster = cluster_pair,
                        col_i = col_i_pair,
                        col_j = col_j_pair)
  cluster_n <- aggregate(col_i ~ cluster,
                         data = pair_df,
                         FUN = length)
  names(cluster_n)[names(cluster_n) == 'col_i'] <- 'weight'

  if (centering_rows == 'all_available') {
    clusters <- as.character(cluster_n$cluster)
    mean_i_by_cluster <- cluster_mean_lookup(col_i, cluster_var)
    mean_j_by_cluster <- cluster_mean_lookup(col_j, cluster_var)
    means <- data.frame(cluster = cluster_n$cluster,
                        col_i = as.numeric(mean_i_by_cluster[clusters]),
                        col_j = as.numeric(mean_j_by_cluster[clusters]))
  } else {
    means <- aggregate(cbind(col_i, col_j) ~ cluster,
                       data = pair_df,
                       FUN = mean)
  }

  means <- merge(means, cluster_n, by = 'cluster', sort = FALSE)

  weights <- NULL
  if (between_weighting == 'cluster_size') {
    weights <- means$weight
  }

  list(col_i = means$col_i,
       col_j = means$col_j,
       weights = weights,
       n_comparisons = nrow(means),
       k_pair = nrow(means),
       n_obs = n_comparisons,
       n_clusters = nrow(means))
}

cluster_mean_lookup <- function(col, cluster_var) {
  valid_rows <- complete.cases(col, cluster_var)
  tapply(col[valid_rows], as.factor(cluster_var[valid_rows]), mean)
}

cluster_means_for_pair_rows <- function(col, cluster_var, cluster_pair) {
  mean_by_cluster <- cluster_mean_lookup(col, cluster_var)
  as.numeric(mean_by_cluster[as.character(cluster_pair)])
}

append_cor_warning <- function(current_warning, new_warning) {
  if (is.null(current_warning) ||
      is.na(current_warning) ||
      current_warning == 'None') {
    return(new_warning)
  }

  paste(current_warning, new_warning, sep = '; ')
}

correlation_pair_state <- function(col_i, col_j, level) {
  if (length(col_i) != length(col_j)) {
    stop("Prepared correlation vectors must have equal lengths.", call. = FALSE)
  }

  if (length(col_i) < 2L) {
    reason <- if (level == 'between') {
      'fewer_than_two_contributing_clusters'
    } else {
      'fewer_than_two_complete_observations'
    }
    return(list(status = 'not_estimable', reason = reason))
  }

  if (any(!is.finite(col_i)) || any(!is.finite(col_j))) {
    return(list(status = 'not_estimable',
                reason = 'non_finite_prepared_values'))
  }

  variance_i <- var(col_i)
  variance_j <- var(col_j)
  zero_i <- !is.finite(variance_i) || variance_i <= 0
  zero_j <- !is.finite(variance_j) || variance_j <= 0
  if (zero_i || zero_j) {
    reason <- if (zero_i && zero_j) {
      'zero_variance_both'
    } else if (zero_i) {
      'zero_variance_parameter1'
    } else {
      'zero_variance_parameter2'
    }
    return(list(status = 'not_estimable', reason = reason))
  }

  list(status = 'ok', reason = NA_character_)
}

correlation_inference_state <- function(status,
                                        requested_inference,
                                        level,
                                        between_inference,
                                        method_selected,
                                        weighted_between_analytic,
                                        degrees_freedom,
                                        p_value,
                                        lower_bound,
                                        upper_bound,
                                        n_boot_valid,
                                        n_clusters,
                                        nboot) {
  if (weighted_between_analytic) {
    return(list(status = 'unavailable',
                reason = 'weighted_analytic_inference_unsupported'))
  }

  between_analytic_omitted <- requested_inference == 'analytic' &&
    level == 'between' &&
    between_inference == 'none'
  if (requested_inference == 'none' || between_analytic_omitted) {
    return(list(status = 'not_requested', reason = NA_character_))
  }

  if (status != 'ok') {
    return(list(status = 'unavailable',
                reason = 'coefficient_not_estimable'))
  }

  if (requested_inference == 'cluster_bootstrap') {
    if (!is.na(n_clusters) && n_clusters < 3L) {
      return(list(status = 'unavailable',
                  reason = 'fewer_than_three_clusters_for_bootstrap'))
    }
    if (is.na(n_boot_valid) || n_boot_valid < 10L) {
      return(list(status = 'unavailable',
                  reason = 'fewer_than_ten_valid_bootstrap_replicates'))
    }
    if (!is.finite(lower_bound) || !is.finite(upper_bound)) {
      return(list(status = 'unavailable',
                  reason = 'bootstrap_interval_unavailable'))
    }
    if (n_boot_valid < nboot) {
      return(list(status = 'partial',
                  reason = 'invalid_bootstrap_replicates_excluded'))
    }
    return(list(status = 'ok', reason = NA_character_))
  }

  if (method_selected != 'pearson') {
    return(list(status = 'unavailable',
                reason = 'analytic_inference_unsupported_for_method'))
  }

  p_available <- length(p_value) == 1L && is.finite(p_value)
  interval_available <- length(lower_bound) == 1L &&
    length(upper_bound) == 1L &&
    is.finite(lower_bound) &&
    is.finite(upper_bound)
  if (p_available && interval_available) {
    return(list(status = 'ok', reason = NA_character_))
  }
  if (p_available) {
    return(list(status = 'partial',
                reason = 'confidence_interval_unavailable_low_df'))
  }

  list(status = 'unavailable',
       reason = if (!is.na(degrees_freedom) && degrees_freedom <= 0) {
         'analytic_inference_unavailable_low_df'
       } else {
         'analytic_inference_unavailable'
       })
}

cluster_bootstrap_statistics <- function(input_data,
                                         column_i,
                                         column_j,
                                         cluster_var,
                                         level,
                                         between_weighting,
                                         centering_rows,
                                         method,
                                         confidence_level,
                                         nboot,
                                         observed_correlation) {
  if (is.null(cluster_var)) {
    stop("cluster_var is required for cluster bootstrap inference.")
  }
  if (method == 'spearman-jackknife') {
    stop("Use method = 'spearman' with inference = 'cluster_bootstrap'.")
  }

  cluster_factor <- droplevels(as.factor(cluster_var))
  clusters <- levels(cluster_factor)
  n_clusters <- length(clusters)

  if (n_clusters < 3 || is.na(observed_correlation)) {
    return(list(correlation_coefficient = observed_correlation,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA,
                n_boot_attempted = 0L,
                n_boot_valid = 0L))
  }

  boot_correlations <- rep(NA_real_, nboot)
  for (boot_idx in seq_len(nboot)) {
    sampled_clusters <- sample(clusters, size = n_clusters, replace = TRUE)
    boot_i <- numeric(0)
    boot_j <- numeric(0)
    boot_cluster <- character(0)

    for (sample_idx in seq_along(sampled_clusters)) {
      rows <- which(cluster_factor == sampled_clusters[sample_idx])
      boot_i <- c(boot_i, input_data[[column_i]][rows])
      boot_j <- c(boot_j, input_data[[column_j]][rows])
      boot_cluster <- c(boot_cluster, rep(paste0('boot_', sample_idx),
                                          length(rows)))
    }

    boot_pair <- prepare_correlation_pair(boot_i,
                                          boot_j,
                                          as.factor(boot_cluster),
                                          level,
                                          between_weighting,
                                          centering_rows)
    boot_correlations[boot_idx] <- correlation_from_pair(boot_pair, method)
  }

  valid_boot <- boot_correlations[is.finite(boot_correlations)]
  if (length(valid_boot) < 10) {
    return(list(correlation_coefficient = observed_correlation,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA,
                n_boot_attempted = nboot,
                n_boot_valid = length(valid_boot)))
  }

  alpha <- 1 - confidence_level
  lower_bound <- as.numeric(quantile(valid_boot,
                                     probs = alpha / 2,
                                     na.rm = TRUE,
                                     names = FALSE))
  upper_bound <- as.numeric(quantile(valid_boot,
                                     probs = 1 - alpha / 2,
                                     na.rm = TRUE,
                                     names = FALSE))
  list(correlation_coefficient = observed_correlation,
       test_statistic = NA,
       p_value = NA_real_,
       lower_bound = lower_bound,
       upper_bound = upper_bound,
       n_boot_attempted = nboot,
       n_boot_valid = length(valid_boot))
}

correlation_from_pair <- function(pair_data, method) {
  if (length(pair_data$col_i) < 2 ||
      sum(!is.na(pair_data$col_i) & !is.na(pair_data$col_j)) < 2) {
    return(NA_real_)
  }

  if (is.null(pair_data$weights)) {
    return(suppressWarnings(cor(pair_data$col_i,
                                pair_data$col_j,
                                method = method)))
  }

  weighted_cor(pair_data$col_i,
               pair_data$col_j,
               pair_data$weights,
               method)
}

correlation_degrees_freedom <- function(method,
                                        level,
                                        n_comparisons,
                                        k_pair,
                                        n_clusters_between) {
  if (method == 'pearson') {
    if (level == 'within') {
      return(n_comparisons - k_pair - 1)
    }
    if (level == 'between') {
      return(k_pair - 2)
    }
    if (!is.null(n_clusters_between)) {
      return(n_clusters_between - 2)
    }
    return(n_comparisons - 2)
  }

  if (method == 'spearman') {
    if (level == 'within') {
      return(n_comparisons - k_pair - 1)
    }
    if (level == 'between') {
      return(k_pair - 3)
    }
    if (!is.null(n_clusters_between)) {
      return(n_clusters_between - 3)
    }
    return(n_comparisons - 3)
  }

  NA
}
