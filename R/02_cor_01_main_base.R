
#######################################################
# Calculating Statistics
#######################################################
#' @importFrom stats pnorm qnorm var
# This function calculates the correlation coefficients, p-values, and confidence intervals for the input data.
corAndPValues <- function(input_data,
                          cluster,
                          level,
                          confidence_level = 0.95,
                          method = "pearson",
                          auto_type,
                          var_type,
                          warnings,
                          inference,
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

    # remove all pairwise cases when one variable is missing.
    complete_cases <- complete.cases(col_i, col_j)
    col_i <- col_i[complete_cases]
    col_j <- col_j[complete_cases]
    cluster_ij <- cluster[complete_cases]
    n_comparisons <- sum(complete_cases)
    n_clusters <- length(unique(cluster_ij))

    if (level == 'between') { # for between - person correlations
      base_for_degrees_freedom <- n_clusters
    } else { # for within- person correlations: each cluster mean costs one df
      base_for_degrees_freedom <- n_comparisons - n_clusters + 1
    }


    # set method
    if (auto_type) {
      type_i <- var_type[[i]]
      type_j <- var_type[[j]]

      method <- 'pearson'
      method_table <- "pearson's r"


      if (type_i == 'ordinal' | type_j == 'ordinal') {
        method <- 'spearman'
        method_table <- "spearman's rho"
      } else {
        if(type_i == 'binary' | type_j == 'binary') {
          method <- 'pearson'
          method_table <- "point-biserial"
          if (type_i == 'binary' & type_j  == 'binary') {
            method <- 'pearson'
            method_table <- "phi coefficient"
          }
        }
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
      if (!auto_type) {
        method_table <- "pearson's r"
      }
    } else if (method == 'spearman') {
      degrees_freedom <- base_for_degrees_freedom - 3
      statistic_type <- 'z-statistic'
      if (!auto_type) {
        method_table <- "spearman's rho"
      }
    }

    # Ignore correlations with fewer than 3 units (two units always correlate perfectly).
    if (base_for_degrees_freedom < 3) {
      degrees_freedom <- NA
      correlations_statistics_list <- list(correlation_coefficient = NA,
                                           test_statistic = NA,
                                           p_value = NA,
                                           lower_bound = NA,
                                           upper_bound = NA)
    } else {
      correlations_statistics_list <- calculate_correlations_and_statistics(col_i, col_j,
                                                                            cluster_ij,
                                                                            method,
                                                                            degrees_freedom,
                                                                            confidence_level,
                                                                            inference,
                                                                            nboot)
    }

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



  result_table <- format_result_table(result_table, method, auto_type, var_type, confidence_level, inference)

  return(list(p_value = p_value_df,
              correlation_coefficient = correlation_coefficient_df,
              confidence_intervals = conf_int_df,
              result_table = result_table))
}
