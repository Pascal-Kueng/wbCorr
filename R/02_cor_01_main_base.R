
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
                          centering_rows = c('pairwise_complete', 'all_available'),
                          inference = c('analytic', 'none', 'cluster_bootstrap')) {
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


    # Check if there are enough finite observations
    if (is.null(col_i) || is.null(col_j) ||
        (sum(!is.na(col_i) & !is.na(col_j))) < 3 ) { # Ignore correlations with insufficient finite observations
      cor_matrix[i, j] <- cor_matrix[j, i] <- NA
      p_matrix[i, j] <- p_matrix[j, i] <- NA
      temp_ci_df <- data.frame(Parameter1 = names(input_data[i]),
                               Parameter2 = names(input_data[j]),
                               CI_lower = NA,
                               correlation_coefficient = NA,
                               CI_upper = NA)
      conf_int_df <- rbind(conf_int_df, temp_ci_df)
      next
    }

    # set method
    method_selected <- method
    if (auto_type) {
      type_i <- var_type[[names(input_data)[i]]]
      type_j <- var_type[[names(input_data)[j]]]

      method_selected <- 'pearson'
      method_table <- "pearson's r"


      if (type_i == 'ordinal' | type_j == 'ordinal') {
        method_selected <- 'spearman'
        method_table <- "spearman's rho"
      } else {
        if(type_i == 'binary' | type_j == 'binary') {
          method_selected <- 'pearson'
          method_table <- "point-biserial"
          if (type_i == 'binary' & type_j  == 'binary') {
            method_selected <- 'pearson'
            method_table <- "phi coefficient"
          }
        }
      }
    } else if (method_selected == 'pearson') {
      method_table <- "pearson's r"
    } else if (method_selected %in% c('spearman', 'spearman-jackknife')) {
      method_table <- "spearman's rho"
    }

    warning_i <- warnings[[names(input_data)[i]]]
    warning_j <- warnings[[names(input_data)[j]]]

    auto_warning <- 'None'
    if (!warning_j == 'None') {
      auto_warning <- warning_j
    }
    if (!warning_i == 'None') {
      auto_warning <- warning_i
    }

    # Set degrees freedom
    if (method_selected == 'pearson') {
      degrees_freedom <- correlation_degrees_freedom(method_selected,
                                                     level,
                                                     n_comparisons,
                                                     k_pair,
                                                     n_clusters_between)
      statistic_type <- 't-statistic'
    } else if (method_selected == 'spearman') {
      degrees_freedom <- correlation_degrees_freedom(method_selected,
                                                     level,
                                                     n_comparisons,
                                                     k_pair,
                                                     n_clusters_between)
      statistic_type <- 'z-statistic'
    } else if (method_selected == 'spearman-jackknife') {
      degrees_freedom <- NA
      statistic_type <- NA
    }

    else {
      degrees_freedom <- NA
      statistic_type <- NA
    }

    if (inference == 'none' ||
        (level == 'between' && between_inference == 'none')) {
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
                                           test_statistic = NA,
                                           p_value = NA,
                                           lower_bound = NA,
                                           upper_bound = NA)
      statistic_type <- NA
      degrees_freedom <- NA
    } else if (inference == 'cluster_bootstrap') {
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
        observed_correlation = if (is.null(weights)) {
          suppressWarnings(cor(col_i, col_j, method = method_selected))
        } else {
          weighted_cor(col_i, col_j, weights, method_selected)
        }
      )
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

    if (level == 'between' &&
        between_weighting == 'cluster_size' &&
        inference == 'analytic' &&
        between_inference == 'analytic' &&
        method_selected != 'spearman-jackknife') {
      auto_warning <- append_cor_warning(auto_warning,
                                         'weighted between inference approximate')
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
    temp_ci_df <- data.frame(Parameter1 = names(input_data[i]),
                             Parameter2 = names(input_data[j]),
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
    temp_df <- data.frame(Parameter1 = names(input_data[i]),
                          Parameter2 = names(input_data[j]),
                          warning = auto_warning,
                          method = method_table,
                          coefficient = round(correlation_coefficient, 2),
                          statistic_type = statistic_type,
                          statistic = round(test_statistic, 2),
                          df = degrees_freedom,
                          CI = ci_text,
                          p = p_value)
    result_table <- rbind(result_table, temp_df)
  }

  # if there is no variance on a variable, set correlation with itself to NA.
  for (i in 1:n_numeric) {
    col_i <- input_data[[i]]

    # Check if the variable has zero variance
    if (is.na(var(col_i, na.rm = TRUE)) || var(col_i, na.rm = TRUE) == 0) {
      cor_matrix[i, i] <- NA
      p_matrix[i,i] <- NA
    }
  }

  # Converting the other matrices to DFs
  p_value_df <- as.data.frame(p_matrix)
  correlation_coefficient_df <- as.data.frame(cor_matrix)



  result_table <- format_result_table(result_table,
                                      method,
                                      auto_type,
                                      var_type,
                                      confidence_level,
                                      inference)

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
                k_pair = NA_integer_))
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
                k_pair = 0))
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
                k_pair = k_pair))
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
       k_pair = nrow(means))
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
                upper_bound = NA))
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

  valid_boot <- boot_correlations[!is.na(boot_correlations)]
  if (length(valid_boot) < 10) {
    return(list(correlation_coefficient = observed_correlation,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA))
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
  p_value <- min(2 * min(mean(valid_boot <= 0),
                         mean(valid_boot >= 0)),
                 1)

  list(correlation_coefficient = observed_correlation,
       test_statistic = NA,
       p_value = p_value,
       lower_bound = lower_bound,
       upper_bound = upper_bound)
}

correlation_from_pair <- function(pair_data, method) {
  if (length(pair_data$col_i) < 3 ||
      sum(!is.na(pair_data$col_i) & !is.na(pair_data$col_j)) < 3) {
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
