

calculate_correlations_and_statistics <- function(col_i, col_j,
                                                  method,
                                                  degrees_freedom,
                                                  confidence_level,
                                                  bootstrap,
                                                  nboot,
                                                  weights = NULL) {

  # boot
  if (bootstrap) {
    if (!is.null(weights)) {
      stop("Bootstrap inference is not supported for weighted correlations.")
    }
    return(cor_bootstrap(col_i, col_j, method, confidence_level, nboot))
  }

  if (!is.null(weights)) {
    correlation_coefficient <- weighted_cor(col_i, col_j, weights, method)
    if (method == 'pearson') {
      return(cor_pearson_from_r(correlation_coefficient,
                                degrees_freedom,
                                confidence_level))
    } else if (method == 'spearman') {
      return(cor_spearman_from_r(correlation_coefficient,
                                 degrees_freedom,
                                 confidence_level))
    }
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

weighted_cor <- function(col_i, col_j, weights, method = 'pearson') {
  complete_cases <- complete.cases(col_i, col_j, weights)
  col_i <- col_i[complete_cases]
  col_j <- col_j[complete_cases]
  weights <- weights[complete_cases]

  if (length(col_i) < 3 || sum(weights) <= 0) {
    return(NA_real_)
  }

  if (method == 'spearman') {
    col_i <- rank(col_i, ties.method = 'average')
    col_j <- rank(col_j, ties.method = 'average')
  }

  mean_i <- sum(weights * col_i) / sum(weights)
  mean_j <- sum(weights * col_j) / sum(weights)
  centered_i <- col_i - mean_i
  centered_j <- col_j - mean_j

  denominator <- sqrt(sum(weights * centered_i^2) *
                        sum(weights * centered_j^2))
  if (denominator == 0) {
    return(NA_real_)
  }

  sum(weights * centered_i * centered_j) / denominator
}
