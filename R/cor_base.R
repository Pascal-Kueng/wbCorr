calculate_correlations_and_statistics <- function(col_i, col_j,
                                                  method, method1,
                                                  n_clusters_between,
                                                  n_comparisons,
                                                  alpha_level) {

  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = method1))
  if (is.na(correlation_coefficient)) {
    test_statistic <- NA
    p_value <- NA
    lower_bound <- NA
    upper_bound <- NA
    degrees_freedom <- NA
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
      jack <- c(NA, NA)
      message("sampling... ")
      tryCatch(jack <- jackknife(col_i, col_j))
      lower_bound <- jack[1]
      upper_bound <- jack[2]
      p_value <- NA # Extract the p_value from the jackknife function's output
      test_statistic <- NA
    }
  }
  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              degrees_freedom = degrees_freedom,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}

