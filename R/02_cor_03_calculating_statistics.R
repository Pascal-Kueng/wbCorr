

calculate_correlations_and_statistics <- function(col_i, col_j,
                                                  cluster,
                                                  method,
                                                  degrees_freedom,
                                                  confidence_level,
                                                  inference,
                                                  nboot) {

  # boot
  if (inference == 'cluster_bootstrap') {
    return(cor_bootstrap(col_i, col_j, cluster, method, confidence_level, nboot))
  }

  # calculating correlations and statistics
  if (method == 'pearson') {
    results <- cor_pearson(col_i, col_j, degrees_freedom, confidence_level)
  } else if (method == 'spearman') {
    results <- cor_spearman(col_i, col_j, degrees_freedom, confidence_level)
  }

  # coefficients only
  if (inference == 'none') {
    results[c('test_statistic', 'p_value', 'lower_bound', 'upper_bound')] <- NA
  }
  return(results)
}

