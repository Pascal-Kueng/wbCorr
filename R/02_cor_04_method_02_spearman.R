
cor_spearman <- function(degrees_freedom, confidence_level, correlation_coefficient) {
  # Fisher Z-transformation
  se <- 1 / sqrt(degrees_freedom)

  critical_value = qnorm(1 - (1 - confidence_level) / 2)
  delta <- critical_value * se

  z_score <- atanh(correlation_coefficient)
  lower_bound <- tanh(z_score - delta)
  upper_bound <- tanh(z_score + delta)

  # p-values
  test_statistic <- z_score / se
  p_value <- 2 * pnorm(abs(test_statistic), lower.tail = FALSE)

  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}

