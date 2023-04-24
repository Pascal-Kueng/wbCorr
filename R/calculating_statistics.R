
#######################################################
# Calculating Statistics
#######################################################

# This function calculates the correlation coefficients, p-values, and confidence intervals for the input data.
corAndPValues <- function(input_data, n_clusters_between = NULL, alpha_level = 0.95, method = "pearson") {
  data_numeric <- input_data

  n_numeric <- ncol(input_data)
  p_matrix <- matrix(1,
                     ncol = n_numeric, nrow = n_numeric,
                     dimnames = list(names(input_data), names(input_data))
  )
  cor_matrix <- matrix(1,
                       ncol = n_numeric, nrow = n_numeric,
                       dimnames = list(names(input_data), names(input_data))
  )
  conf_int_matrix <- array(NA,
                           dim = c(n_numeric, n_numeric, 2),
                           dimnames = list(names(input_data), names(input_data), c("lower", "upper"))
  )

  idx_combinations <- t(combn(n_numeric, 2))

  result_table <- data.frame(Parameter1 = numeric(0),
                             Parameter2 = numeric(0),
                             r = numeric(0),
                             CI_lower = numeric(0),
                             CI_upper = numeric(0),
                             t = numeric(0),
                             p = numeric(0))

  for (k in 1:nrow(idx_combinations)) {
    i <- idx_combinations[k, 1]
    j <- idx_combinations[k, 2]

    col_i <- data_numeric[[i]]
    col_j <- data_numeric[[j]]

    if (is.null(col_i) || is.null(col_j)) { # Ignore correlations with null columns
      cor_matrix[i, j] <- cor_matrix[j, i] <- NA
      p_matrix[i, j] <- p_matrix[j, i] <- NA
      conf_int_matrix[i, j, ] <- conf_int_matrix[j, i, ] <- NA
      next
    }

    # Check if there are enough finite observations
    finite_count <- sum(!is.na(col_i) & !is.na(col_j))
    if (finite_count < 3) { # Ignore correlations with insufficient finite observations
      cor_matrix[i, j] <- cor_matrix[j, i] <- NA
      p_matrix[i, j] <- p_matrix[j, i] <- NA
      conf_int_matrix[i, j, ] <- conf_int_matrix[j, i, ] <- NA
      next
    }

    # Retrieve the correlation coefficient
    r <- suppressWarnings(cor(col_i, col_j,
                              method = method,
                              use = 'pairwise.complete.obs'))

    if (!is.null(n_clusters_between)) { # for between - person correlations
      degrees_freedom <- n_clusters_between - 2
    } else { # for within- person correlations
      # extract number of complete pairs that were compared to calculate DF
      complete_cases <- complete.cases(col_i, col_j)
      n_comparisons <- sum(complete_cases)
      degrees_freedom <- n_comparisons - 2
    }

    # Compute confidence intervals and p-values using t-distribution
    t_score <- qt((1 + alpha_level) / 2, df = degrees_freedom)
    t_statistic <- r * sqrt(degrees_freedom / (1 - r^2))
    p_value <- 2 * (1 - pt(abs(t_statistic), df = degrees_freedom, lower.tail = TRUE))

    lower_bound <- r - t_score * sqrt((1 - r^2) / (degrees_freedom))
    upper_bound <- r + t_score * sqrt((1 - r^2) / (degrees_freedom))

    # populate matrices
    cor_matrix[i, j] <- cor_matrix[j, i] <- r
    p_matrix[i, j] <- p_matrix[j, i] <- p_value
    conf_int_matrix[i, j, "lower"] <- conf_int_matrix[j, i, "lower"] <- lower_bound
    conf_int_matrix[i, j, "upper"] <- conf_int_matrix[j, i, "upper"] <- upper_bound

    # populate dataframe
    temp_df <- data.frame(Parameter1 = names(input_data[i]),
                          Parameter2 = names(input_data[j]),
                          r = round(r, 2),
                          CI = sprintf('[%0.2f, %0.2f]', lower_bound, upper_bound),
                          t = round(t_statistic, 2),
                          p = p_value)
    result_table <- rbind(result_table, temp_df)
  }
  # Converting the other matrices to DFs
  p_value_df <- as.data.frame(p_matrix)
  correlation_coefficient_df <- as.data.frame(cor_matrix)
  conf_int_df <- array2table(conf_int_matrix)

  # rename columns and formatting main table
  names(result_table)[5] <- c(sprintf('t(%d)', degrees_freedom))
  names(result_table)[4] <- c('95% CI')

  result_table$p <- ifelse(result_table$p < .001, "< .001***",
                           ifelse(result_table$p < .01, sprintf("%.3f**", result_table$p),
                                  ifelse(result_table$p < .05, sprintf("%.3f*", result_table$p),
                                         sprintf("%.3f", result_table$p))))

  return(list(p_value = p_value_df, correlation_coefficient = correlation_coefficient_df, confidence_intervals = conf_int_df, result_table = result_table))
}


# Helper function to convert an array into a data frame with multi-index columns
array2table <- function(array) {
  dims <- dim(array)
  new_dims <- c(prod(dims[1:2]), dims[3])
  array_reshaped <- array(data = array, dim = new_dims)
  array_df <- as.data.frame(array_reshaped)
  array_df$Var1 <- NULL
  colnames(array_df) <- dimnames(array)[[3]]
  rownames(array_df) <- do.call(paste, c(expand.grid(dimnames(array)[1:2]), sep = ":"))
  return(array_df)
}


