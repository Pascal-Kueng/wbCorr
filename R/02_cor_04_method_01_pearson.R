cor_pearson <- function(col_i, col_j, degrees_freedom, confidence_level) {
  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = "pearson"))

  cor_pearson_from_r(correlation_coefficient,
                     degrees_freedom,
                     confidence_level)
}


cor_pearson_from_r <- function(correlation_coefficient,
                               degrees_freedom,
                               confidence_level) {
  empty_result <- function(coefficient) {
    list(correlation_coefficient = coefficient,
         test_statistic = NA_real_,
         p_value = NA_real_,
         lower_bound = NA_real_,
         upper_bound = NA_real_)
  }

  if (length(correlation_coefficient) != 1L ||
        is.na(correlation_coefficient) ||
        !is.finite(correlation_coefficient) ||
        abs(correlation_coefficient) > 1 + sqrt(.Machine$double.eps)) {
    return(empty_result(NA_real_))
  }
  correlation_coefficient <- max(-1, min(1, correlation_coefficient))

  # Keep an estimable coefficient even when there are too few degrees of
  # freedom for analytic inference.
  if (length(degrees_freedom) != 1L ||
        is.na(degrees_freedom) ||
        !is.finite(degrees_freedom) ||
        degrees_freedom <= 0) {
    return(empty_result(correlation_coefficient))
  }

  if (abs(correlation_coefficient) == 1) {
    test_statistic <- sign(correlation_coefficient) * Inf
    p_value <- 0
  } else {
    test_statistic <- correlation_coefficient *
      sqrt(degrees_freedom / (1 - correlation_coefficient^2))
    p_value <- 2 * pt(abs(test_statistic),
                      df = degrees_freedom,
                      lower.tail = FALSE)
  }

  # Fisher's z interval requires an effective sample size of at least four,
  # which corresponds to t degrees of freedom greater than one.
  if (degrees_freedom <= 1) {
    lower_bound <- NA_real_
    upper_bound <- NA_real_
  } else if (abs(correlation_coefficient) == 1) {
    lower_bound <- correlation_coefficient
    upper_bound <- correlation_coefficient
  } else {
    critical_value <- qnorm((1 + confidence_level) / 2)
    standard_error_z <- 1 / sqrt(degrees_freedom - 1)
    interval_z <- atanh(correlation_coefficient) +
      c(-1, 1) * critical_value * standard_error_z
    interval <- tanh(interval_z)
    lower_bound <- interval[1]
    upper_bound <- interval[2]
  }

  list(correlation_coefficient = correlation_coefficient,
       test_statistic = test_statistic,
       p_value = p_value,
       lower_bound = lower_bound,
       upper_bound = upper_bound)
}
