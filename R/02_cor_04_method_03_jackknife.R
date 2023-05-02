cor_jackknife <- function(col_i, col_j, confidence_level) {
  set.seed(42)

  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = 'spearman'))

  if (is.na(correlation_coefficient)) {
    return(list(correlation_coefficient = NA,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA))
  }

  if (!all(lengths(list(col_i, col_j)) == length(col_i))) {
    stop("Input vectors must have the same length.")
  }

  n_comparisons <- length(col_j)

  jackknife_pseudo_values <- mapply(function(i, j) {
    n_comparisons * correlation_coefficient - (n_comparisons - 1) * cor(col_i[-i], col_j[-j], method = "spearman")
  }, i = seq_along(col_i), j = seq_along(col_j))

  variance_estimate <- function(theta) {
    1 / n_comparisons * sum((jackknife_pseudo_values - theta)^2)
  }

  test_statistic <- function(theta) {
    n_comparisons * (correlation_coefficient - theta)^2 / variance_estimate(theta) - qchisq(confidence_level, 1)
  }

  lower <- tryCatch({
    uniroot(test_statistic, interval = c(-1, correlation_coefficient), tol = 1e-10)$root
  }, error = function(e) {
    NA
  })

  upper <- tryCatch({
    uniroot(test_statistic, interval = c(correlation_coefficient, 1), tol = 1e-10)$root
  }, error = function(e) {
    NA
  })

  return(list(
    correlation_coefficient = correlation_coefficient,
    test_statistic = NA,
    degrees_freedom = NA,
    p_value = NA,
    lower_bound = lower,
    upper_bound = upper
  ))
}
