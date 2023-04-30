

cor_pearson <- function(degrees_freedom, alpha_level, correlation_coefficient) {
  # Compute confidence intervals and p-values using t-distribution
  t_score <- qt((1 + alpha_level) / 2, df = degrees_freedom)
  test_statistic <- correlation_coefficient * sqrt(degrees_freedom / (1 - correlation_coefficient^2))
  p_value <- 2 * pt(abs(test_statistic), df = degrees_freedom, lower.tail = FALSE)

  lower_bound <- correlation_coefficient - t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))
  upper_bound <- correlation_coefficient + t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))

  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              degrees_freedom = degrees_freedom,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}
