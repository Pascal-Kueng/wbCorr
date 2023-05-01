

calculate_correlations_and_statistics <- function(col_i, col_j,
                                                  method,
                                                  degrees_freedom,
                                                  alpha_level,
                                                  bootstrap,
                                                  nboot) {

  # Retrieve the correlation coefficient
  if (method == 'spearman-jackknife') {
    method1 <- 'spearman'
  } else {
    method1 <- method
  }

  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = method1))
  if (is.na(correlation_coefficient)) {
    return(list(correlation_coefficient = NA,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA))
  }

  # boot
  if (bootstrap) {
    return(cor_bootstrap(col_i, col_j, method, alpha_level, nboot, correlation_coefficient))
  }

  # calculating correlations and statistics
  if (method == 'pearson') {
    return(cor_pearson(degrees_freedom, alpha_level, correlation_coefficient))
  } else if (method == 'spearman') {
    return(cor_spearman(degrees_freedom, alpha_level, correlation_coefficient))
  } else if (method == 'spearman-jackknife') {
    return(cor_jackknife(col_i, col_j, alpha_level, correlation_coefficient))
  }
}

