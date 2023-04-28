
#######################################################
# Calculating Statistics
#######################################################
#' @importFrom stats pnorm qchisq qnorm uniroot var
# This function calculates the correlation coefficients, p-values, and confidence intervals for the input data.
corAndPValues <- function(input_data, n_clusters_between = NULL, alpha_level = 0.95, method = "pearson", n_boot = NULL) {
  # initializing values
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

    # Retrieve the correlation coefficient
    if (method == 'spearman-jackknife') {
      method1 <- 'spearman'
    } else {
      method1 <- method
    }

    correlations_statistics_list <- calculate_correlations_and_statistics(col_i, col_j,
                                                                          method, method1,
                                                                          n_clusters_between,
                                                                          n_comparisons,
                                                                          alpha_level)
    class(correlations_statistics_list)
    correlation_coefficient <- correlations_statistics_list$correlation_coefficient
    test_statistic <- correlations_statistics_list$test_statistic
    degrees_freedom <- correlations_statistics_list$degrees_freedom
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
                          coefficient = round(correlation_coefficient, 2),
                          CI = sprintf('[%0.2f, %0.2f]', lower_bound, upper_bound),
                          statistic = round(test_statistic, 2),
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

  # rename columns and formatting main table
  if (method == 'pearson') {
    names(result_table)[5] <- c(sprintf('t(%d)', degrees_freedom))
    names(result_table)[3] <- c("pearson's r")
  } else if (method == 'spearman') {
    names(result_table)[5] <- c(sprintf('z(%d)', degrees_freedom))
    names(result_table)[3] <- c("spearman's rho")
  } else if (method == 'spearman-jackknife') {
    names(result_table)[3] <- c("spearman's rho")

  }

  names(result_table)[4] <- c('95% CI')

  result_table$p <- ifelse(result_table$p < .001, "< .001***",
                           ifelse(result_table$p < .01, sprintf("%.3f**", result_table$p),
                                  ifelse(result_table$p < .05, sprintf("%.3f*", result_table$p),
                                         sprintf("%.3f", result_table$p))))

  return(list(p_value = p_value_df, correlation_coefficient = correlation_coefficient_df, confidence_intervals = conf_int_df, result_table = result_table))
}

initializing_values <- function(input_data) {
  n_numeric <- ncol(input_data)
  p_matrix <- matrix(0,
                     ncol = n_numeric, nrow = n_numeric,
                     dimnames = list(names(input_data), names(input_data)))
  cor_matrix <- matrix(1,
                       ncol = n_numeric, nrow = n_numeric,
                       dimnames = list(names(input_data), names(input_data)))
  conf_int_df <- data.frame(Parameter1 = character(0),
                            Parameter2 = character(0),
                            CI_lower = numeric(0),
                            correlation_coefficient = numeric(0),
                            CI_upper = numeric(0))

  result_table <- data.frame(Parameter1 = numeric(0),
                             Parameter2 = numeric(0),
                             correlation_coefficient = numeric(0),
                             CI_lower = numeric(0),
                             CI_upper = numeric(0),
                             t = numeric(0),
                             p = numeric(0))

  idx_combinations <- t(combn(n_numeric, 2))
  return(list(n_numeric = n_numeric,
              p_matrix = p_matrix,
              cor_matrix = cor_matrix,
              conf_int_df = conf_int_df,
              result_table = result_table,
              idx_combinations = idx_combinations))
}

