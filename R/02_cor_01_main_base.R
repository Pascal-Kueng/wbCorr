
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
                          warnings,
                          bootstrap,
                          nboot) {
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

    col_i <- input_data[[i]]
    col_j <- input_data[[j]]


    # Check if there are enough finite observations
    if (is.null(col_i) || is.null(col_j) || (sum(!is.na(col_i) & !is.na(col_j))) < 3 ) { # Ignore correlations with insufficient finite observations
      cor_matrix[i, j] <- cor_matrix[j, i] <- NA
      p_matrix[i, j] <- p_matrix[j, i] <- NA
      temp_ci_df <- data.frame(Parameter1 = names(input_data[i]),
                               Parameter2 = names(input_data[j]),
                               CI_lower = lower_bound,
                               correlation_coefficient = correlation_coefficient,
                               CI_upper = upper_bound)
      conf_int_df <- rbind(conf_int_df, temp_ci_df)
      next
    }

    # remove all pairwise cases when one variable is missing.
    complete_cases <- complete.cases(col_i, col_j)
    col_i <- col_i[complete_cases]
    col_j <- col_j[complete_cases]
    n_comparisons <- sum(complete_cases)

    if (!is.null(n_clusters_between)) { # for between - person correlations
      base_for_degrees_freedom <- n_clusters_between
    } else { # for within- person correlations
      base_for_degrees_freedom <- n_comparisons
    }


    # set method
    if (!is.null(auto_type)) {
      type_i <- auto_type[[i]]
      type_j <- auto_type[[j]]

      if (type_i == 'factor' | type_j == 'factor') {
        method = 'spearman'
      } else {
        method = 'pearson'
      }
    }

    warning_i <- warnings[[i]]
    warning_j <- warnings[[j]]

    auto_warning <- 'None'
    if (!warning_j == 'None') {
      auto_warning <- warning_j
    }
    if (!warning_i == 'None') {
      auto_warning <- warning_i
    }

    # Set degrees freedom
    if (method == 'pearson') {
      degrees_freedom <- base_for_degrees_freedom -2
      statistic_type <- 't-statistic'
      method_table <- "pearson's r"
    } else if (method == 'spearman') {
      degrees_freedom <- base_for_degrees_freedom - 3
      statistic_type <- 'z-statistic'
      method_table <- "spearman's rho"
    } else if (method == 'spearman-jackknife') {
      degrees_freedom <- NA
      statistic_type <- NA
      method_table <- "spearman's rho"
    }

    else {
      degrees_freedom <- NA
      statistic_type <- NA
    }

    correlations_statistics_list <- calculate_correlations_and_statistics(col_i, col_j,
                                                                          method,
                                                                          degrees_freedom,
                                                                          confidence_level,
                                                                          bootstrap,
                                                                          nboot)

    correlation_coefficient <- correlations_statistics_list$correlation_coefficient
    test_statistic <- correlations_statistics_list$test_statistic
    p_value <- correlations_statistics_list$p_value
    lower_bound <- correlations_statistics_list$lower_bound
    upper_bound <- correlations_statistics_list$upper_bound

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

    # populate big dataframe
    temp_df <- data.frame(Parameter1 = names(input_data[i]),
                          Parameter2 = names(input_data[j]),
                          warning = auto_warning,
                          method = method_table,
                          coefficient = round(correlation_coefficient, 2),
                          statistic_type = statistic_type,
                          statistic = round(test_statistic, 2),
                          df = degrees_freedom,
                          CI = sprintf('[%0.2f, %0.2f]', lower_bound, upper_bound),
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



  result_table <- format_result_table(result_table, method, auto_type, confidence_level, bootstrap)

  return(list(p_value = p_value_df,
              correlation_coefficient = correlation_coefficient_df,
              confidence_intervals = conf_int_df,
              result_table = result_table))
}
