

cor_pearson <- function(col_i, col_j, degrees_freedom, confidence_level) {


  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = 'pearson'))

  if (is.na(correlation_coefficient)) {
    return(list(correlation_coefficient = NA,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA))
  }

  # p-values using t-distribution
  test_statistic <- correlation_coefficient * sqrt(degrees_freedom / (1 - correlation_coefficient^2))
  p_value <- 2 * pt(abs(test_statistic), df = degrees_freedom, lower.tail = FALSE)

  # Confidence intervals using Fisher z-transformation (stays within -1 and 1)
  se <- 1 / sqrt(degrees_freedom - 1)
  delta <- qnorm(1 - (1 - confidence_level) / 2) * se
  z_score <- atanh(correlation_coefficient)
  lower_bound <- tanh(z_score - delta)
  upper_bound <- tanh(z_score + delta)

  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}
