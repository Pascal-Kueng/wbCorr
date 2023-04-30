
cor_jackknife <- function(col_i, col_j, alpha_level, correlation_coefficient) {
  message('sampling... ')
  set.seed('42')
  tryCatch({
    if (length(col_i) != length(col_j)) {
      stop("Input vectors must have the same length.")
    }

    n_comparisons <- length(col_j)

    jackknife_pseudo_values <- as.double()
    for(index in 1:n_comparisons) {
      jackknife_pseudo_values[index] <- n_comparisons * correlation_coefficient - (n_comparisons - 1) * cor(col_i[-index], col_j[-index], method = "spearman")
    }

    variance_estimate <- function(theta) {
      1 / n_comparisons * sum((jackknife_pseudo_values - theta)^2)
    }

    test_statistic <- function(theta) {
      n_comparisons * (correlation_coefficient - theta)^2 / variance_estimate(theta) - qchisq(alpha_level, 1)
    }
    lower <- NA
    upper <- NA

    tryCatch({
      lower <- uniroot(test_statistic, interval = c(-1, correlation_coefficient), tol = 1e-10)$root
    }, error = function(e) {
      lower <- NA
    })
    tryCatch({
      upper <- uniroot(test_statistic, interval = c(correlation_coefficient, 1), tol = 1e-10)$root
    }, error = function(e) {
      upper <- NA
    })

    return(list(correlation_coefficient = correlation_coefficient,
                test_statistic = NA,
                degrees_freedom = NA,
                p_value = NA,
                lower_bound = lower,
                upper_bound = upper))
  }, error = function(e) {
    return(list(correlation_coefficient = correlation_coefficient,
                test_statistic = NA,
                degrees_freedom = NA,
                p_value = NA,
                lower_bound = 444,
                upper_bound = 444))
    })
  }
