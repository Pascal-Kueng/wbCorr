
cor_bootstrap <- function(col_i, col_j, method, confidence_level, nboot, correlation_coefficient) {
  set.seed(42)
  # Combine data
  df <- data.frame(x = col_i, y = col_j)

  # Bootstrap
  boot_indices <- replicate(nboot,
                            sample(nrow(df), replace = TRUE),
                            simplify = FALSE)
  boot_corr <- sapply(boot_indices,
                      function(indices) cor(df[indices, 1], df[indices, 2], method = method))

  # Calculate bias and acceleration
  z0 <- qnorm(mean(boot_corr < correlation_coefficient))
  a <- sum((mean(boot_corr) - boot_corr)^3) / (6 * (sum((mean(boot_corr) - boot_corr)^2)^(3/2)))

  # Calculate the adjusted critical values
  z_alpha <- qnorm(c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))
  z_alpha_adj1 <- (z0 + (z0 + z_alpha[1])) / (1 - a * (z0 + z_alpha[1]))
  z_alpha_adj2 <- (z0 + (z0 + z_alpha[2])) / (1 - a * (z0 + z_alpha[2]))

  # Calculate the BCa confidence interval
  lower_bound <- quantile(boot_corr, pnorm(z_alpha_adj1))
  upper_bound <- quantile(boot_corr, pnorm(z_alpha_adj2))

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
