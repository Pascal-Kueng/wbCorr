
#' @importFrom stats quantile
bootstrapCI <- function(input_data, n_boot = 1000, alpha_level = 0.95, method = "pearson") {
  set.seed(42)

  # Calculate bootstrap statistics
  bootstrapStats <- function(x, y, method) {
    if (method == "pearson") {
      cor(x, y)
    } else if (method == "spearman") {
      cor(x, y, method = "spearman")
    }
  }

  # Perform bootstrap
  bootstrapCorrelation <- function(col_i, col_j, n_boot, method) {
    boot_samples <- numeric(n_boot)

    for (i in seq_along(boot_samples)) {
      idx <- sample.int(length(col_i), replace = TRUE)
      boot_samples[i] <- bootstrapStats(col_i[idx], col_j[idx], method)
    }

    boot_samples
  }
  # Calculate bootstrap CI
  bootstrapCI <- function(boot_samples, alpha_level) {
    quantile(boot_samples, probs = c((1 - alpha_level) / 2, 1 - (1 - alpha_level) / 2))
  }

  # Calculate bootstrap p-values
  bootstrapPValues <- function(boot_samples, observed_stat) {
    p_value <- mean(abs(boot_samples) >= abs(observed_stat))
    p_value
  }

  boot_samples <- bootstrapCorrelation(col_i, col_j, n_boot, method)
  ci <- bootstrapCI(boot_samples, alpha_level)
  p_value <- bootstrapPValues(boot_samples, correlation_coefficient)
}
