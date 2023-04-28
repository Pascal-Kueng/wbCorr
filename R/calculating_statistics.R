
#######################################################
# Calculating Statistics
#######################################################

# This function calculates the correlation coefficients, p-values, and confidence intervals for the input data.
corAndPValues <- function(input_data, n_clusters_between = NULL, alpha_level = 0.95, method = "pearson") {
  data_numeric <- input_data

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

    # remove all cases when one variable is missing.
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

    correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                    method = method1))
    if (is.na(correlation_coefficient)) {
      test_statistic <- NA
      p_value <- NA
      lower_bound <- NA
      upper_bound <- NA
    } else {
      # degrees of freedom for t-test
      if (!is.null(n_clusters_between)) { # for between - person correlations
        degrees_freedom <- n_clusters_between - 2
      } else { # for within- person correlations
        degrees_freedom <- n_comparisons - 2
      }

      if (method == 'pearson') {
        # Compute confidence intervals and p-values using t-distribution
        t_score <- qt((1 + alpha_level) / 2, df = degrees_freedom)
        test_statistic <- correlation_coefficient * sqrt(degrees_freedom / (1 - correlation_coefficient^2))
        p_value <- 2 * pt(abs(test_statistic), df = degrees_freedom, lower.tail = FALSE)

        lower_bound <- correlation_coefficient - t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))
        upper_bound <- correlation_coefficient + t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))
      } else if (method == 'spearman') {
        # Fisher Z-transformation
        degrees_freedom_z <- degrees_freedom - 1
        se <- 1 / sqrt(degrees_freedom_z)

        critical_value = qnorm(1 - (1 - alpha_level) / 2)
        delta <- critical_value * se

        z_score <- atanh(correlation_coefficient)
        lower_bound <- tanh(z_score - delta)
        upper_bound <- tanh(z_score + delta)

        # p-values
        test_statistic <- z_score / se
        p_value <- 2 * pnorm(abs(test_statistic), lower.tail = FALSE)
      } else if (method == 'spearman-jackknife') {
        print("resampling with jackknife method... ")

        jack <- list(NA, NA)
        try(jack <- jackknife(col_i, col_j))
        lower_bound <- jack[1]
        upper_bound <- jack[2]

        p_value <- NA
        test_statistic <- NA
      }
    }

    # populate matrices
    print("updating matrix...")
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
    col_i <- data_numeric[[i]]

    # Check if the variable has zero variance
    if (var(col_i, na.rm = TRUE) == 0) {
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
    names(result_table)[5] <- c(sprintf('χ2(%d)', degrees_freedom))
  }

  names(result_table)[4] <- c('95% CI')

  result_table$p <- ifelse(result_table$p < .001, "< .001***",
                           ifelse(result_table$p < .01, sprintf("%.3f**", result_table$p),
                                  ifelse(result_table$p < .05, sprintf("%.3f*", result_table$p),
                                         sprintf("%.3f", result_table$p))))

  return(list(p_value = p_value_df, correlation_coefficient = correlation_coefficient_df, confidence_intervals = conf_int_df, result_table = result_table))
}

jackknife <- function(col_i, col_j, alpha_level = 0.95) {
  if (length(col_i) != length(col_j)) {
    stop("Input vectors must have the same length.")
  }
  if (var(col_i == 0) | var(col_j == 0)) {
    return(list(NA, NA))
  }

  length_i <- length(col_i)
  length_j <- length(col_j)

  correlation_coefficient <- cor(col_i, col_j, method = "spearman")
  n_comparisons <- length_i

  jackknife_pseudo_values <- as.double()
  for(index in 1:n_comparisons) {
    jackknife_pseudo_values[index] <- n_comparisons * correlation_coefficient - (n_comparisons - 1) * cor(col_i[-index], col_j[-index], method = "spearman")
  }

  variance_estimate <- function(theta) {
    1 / n_comparisons * sum((jackknife_pseudo_values - theta)^2)
  }

  test_statistic <- function(theta) {
    n_comparisons * (correlation_coefficient - theta)^2 / variance_estimate(theta) - qchisq(alpha_level, 1)
  }

  lower <- uniroot(test_statistic, interval = c(-1, correlation_coefficient), tol = 1e-10)$root
  upper <- uniroot(test_statistic, interval = c(correlation_coefficient, 1), tol = 1e-10)$root

  return(list(lower,upper))
}
