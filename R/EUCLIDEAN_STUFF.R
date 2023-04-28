

jackknife <- function(col_i, col_j, alpha_level = 0.95) {
  length_i <- length(col_i)
  length_j <- length(col_j)

  ## Run a basic input validation
  if (is.vector(col_i) == FALSE | is.vector(col_i) == FALSE | length_i != length_j) {
    stop('col_i and col_j must be vectors of the same length')
  }
  if (sum(is.na(col_i)) != 0 | sum(is.na(col_j)) != 0) {
    stop('missing values are not allowed')
  }

  correlation_coefficient <- cor(col_i, col_j, method = "spearman")
  n_comparisons <- length_i




  jackknife_pseudo_values <- as.double()
  for(index in 1:n_comparisons) {
    jackknife_pseudo_values[index] <- n_comparisons * correlation_coefficient - (n_comparisons - 1) * cor(col_i[-index], col_j[-index],
                                  method = "spearman")
  }

  variance_estimate <- function(theta) {
    1 / n_comparisons * sum((jackknife_pseudo_values - theta)^2)
  }

  test_statistic <- function(theta) {
    n_comparisons * (correlation_coefficient - theta)^2 / variance_estimate(theta) - qchisq(alpha_level, 1) # or 1-alpha_level?
  }

  lower <- uniroot(test_statistic, interval = c(-1, correlation_coefficient), tol = .1*10^{-10})$root
  upper <- uniroot(test_statistic, interval = c(correlation_coefficient, 1), tol = .1*10^{-10})$root

  return(list(lower,upper))
}
