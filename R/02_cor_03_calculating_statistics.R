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

    # calculating correlations and statistics
    if (method == 'pearson') {
      return(cor_pearson(degrees_freedom, alpha_level, correlation_coefficient))
    } else if (method == 'spearman') {
      return(cor_spearman((degrees_freedom - 1), alpha_level, correlation_coefficient))
    } else if (method == 'spearman-jackknife') {
      return(cor_jackknife(col_i, col_j, alpha_level, correlation_coefficient))
    }
  }
  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              degrees_freedom = degrees_freedom,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}

