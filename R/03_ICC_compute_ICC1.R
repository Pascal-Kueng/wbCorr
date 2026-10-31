
# One-way random-effects ICC(1,1) from ANOVA mean squares.
# n0 adjusts the cluster size when clusters are of unequal size.
compute_ICC1 <- function(input_data, cluster_var) {

  ICCs <- data.frame(variable = character(), ICC = numeric()) # Initialize an empty data frame

  for (name in colnames(input_data)) {
    observed <- !is.na(input_data[[name]])
    x <- input_data[[name]][observed]
    cluster <- droplevels(cluster_var[observed])

    N <- length(x)
    k <- nlevels(cluster)
    ICC <- NA
    if (k > 1 && N > k && var(x) > 0) {
      cluster_sizes <- tabulate(cluster)
      cluster_means <- tapply(x, cluster, mean)
      ms_between <- sum(cluster_sizes * (cluster_means - mean(x))^2) / (k - 1)
      ms_within <- sum((x - cluster_means[as.integer(cluster)])^2) / (N - k)
      n0 <- (N - sum(cluster_sizes^2) / N) / (k - 1)
      ICC <- (ms_between - ms_within) / (ms_between + (n0 - 1) * ms_within)
    }

    ICCs <- rbind(ICCs, data.frame(variable = name, ICC = ICC)) # Add the result to the data frame
  }

  return(ICCs)
}
