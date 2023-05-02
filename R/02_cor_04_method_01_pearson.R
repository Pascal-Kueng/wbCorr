

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

  # Compute confidence intervals and p-values using t-distribution
  t_score <- qt((1 + confidence_level) / 2, df = degrees_freedom)
  test_statistic <- correlation_coefficient * sqrt(degrees_freedom / (1 - correlation_coefficient^2))
  p_value <- 2 * pt(abs(test_statistic), df = degrees_freedom, lower.tail = FALSE)

  lower_bound <- correlation_coefficient - t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))
  upper_bound <- correlation_coefficient + t_score * sqrt((1 - correlation_coefficient^2) / (degrees_freedom))

  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = test_statistic,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}
