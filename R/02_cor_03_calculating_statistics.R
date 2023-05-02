

calculate_correlations_and_statistics <- function(col_i, col_j,
                                                  method,
                                                  degrees_freedom,
                                                  confidence_level,
                                                  bootstrap,
                                                  nboot) {

  # boot
  if (bootstrap) {
    return(cor_bootstrap(col_i, col_j, method, confidence_level, nboot))
  }

  # calculating correlations and statistics
  if (method == 'pearson') {
    return(cor_pearson(col_i, col_j, degrees_freedom, confidence_level))
  } else if (method == 'spearman') {
    return(cor_spearman(col_i, col_j, degrees_freedom, confidence_level))
  } else if (method == 'spearman-jackknife') {
    return(cor_jackknife(col_i, col_j, confidence_level))
  }
}

