
cor_spearman <- function(col_i, col_j, degrees_freedom, confidence_level) {

  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = 'spearman'))
  cor_spearman_from_r(correlation_coefficient, degrees_freedom, confidence_level)
}

cor_spearman_from_r <- function(correlation_coefficient, degrees_freedom, confidence_level) {
  if (length(correlation_coefficient) != 1L ||
      is.na(correlation_coefficient) ||
      !is.finite(correlation_coefficient) ||
      abs(correlation_coefficient) > 1 + sqrt(.Machine$double.eps)) {
    correlation_coefficient <- NA_real_
  } else {
    correlation_coefficient <- max(-1, min(1, correlation_coefficient))
  }

  # No analytic sampling distribution is supported for the clustered
  # centered-score and cluster-mean Spearman estimands used by wbCorr.
  list(correlation_coefficient = correlation_coefficient,
       test_statistic = NA_real_,
       p_value = NA_real_,
       lower_bound = NA_real_,
       upper_bound = NA_real_)
}
