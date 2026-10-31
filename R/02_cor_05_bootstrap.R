
#' @importFrom stats quantile
cor_bootstrap <- function(col_i, col_j, cluster, method, confidence_level, nboot) {

  correlation_coefficient <- suppressWarnings(cor(col_i, col_j,
                                                  method = method))
  if (is.na(correlation_coefficient)) {
    return(list(correlation_coefficient = NA,
                test_statistic = NA,
                p_value = NA,
                lower_bound = NA,
                upper_bound = NA))
  }

  # Bootstrap: resample whole clusters, so dependence within clusters is kept.
  rows_by_cluster <- split(seq_along(col_i), droplevels(as.factor(cluster)))
  boot_corr <- replicate(nboot, {
    rows <- unlist(rows_by_cluster[sample.int(length(rows_by_cluster), replace = TRUE)],
                   use.names = FALSE)
    suppressWarnings(cor(col_i[rows], col_j[rows], method = method))
  })
  boot_corr <- boot_corr[!is.na(boot_corr)]

  # Calculate the bias-corrected (BC) confidence interval
  z0 <- qnorm(mean(boot_corr < correlation_coefficient))
  z_alpha <- qnorm(c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))
  lower_bound <- quantile(boot_corr, pnorm(2 * z0 + z_alpha[1]), names = FALSE)
  upper_bound <- quantile(boot_corr, pnorm(2 * z0 + z_alpha[2]), names = FALSE)

  # Calculate the p-value
  p_value <- min(mean((correlation_coefficient > 0 & boot_corr < 0) |
                        (correlation_coefficient < 0 & boot_corr > 0)) * 2, 1)



  # Return the result
  return(list(correlation_coefficient = correlation_coefficient,
              test_statistic = NA,
              p_value = p_value,
              lower_bound = lower_bound,
              upper_bound = upper_bound))
}
