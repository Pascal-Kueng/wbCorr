compute_icc1 <- function(input_data, cluster_var) {
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (length(cluster_var) != nrow(input_data)) {
    stop("cluster_var must have one value per row of input_data")
  }

  icc <- vapply(input_data,
                compute_icc1_anova,
                cluster_var = cluster_var,
                FUN.VALUE = numeric(1))

  data.frame(variable = names(input_data),
             ICC = unname(icc),
             row.names = NULL,
             check.names = FALSE)
}


compute_icc1_anova <- function(values, cluster_var) {
  if (!is.numeric(values)) {
    stop("ICC inputs must be numeric after wbCorr preprocessing.", call. = FALSE)
  }

  valid_rows <- is.finite(values) & !is.na(cluster_var)
  values <- values[valid_rows]
  clusters <- droplevels(as.factor(cluster_var[valid_rows]))

  n_observations <- length(values)
  n_clusters <- nlevels(clusters)

  # ICC(1,1) needs at least two clusters and some within-cluster replication.
  if (n_clusters < 2L || n_observations <= n_clusters) {
    return(NA_real_)
  }

  cluster_sizes <- as.numeric(table(clusters))
  cluster_means <- as.numeric(tapply(values, clusters, mean))
  grand_mean <- mean(values)

  ss_between <- sum(cluster_sizes * (cluster_means - grand_mean)^2)
  ss_within <- sum((values - cluster_means[as.integer(clusters)])^2)

  ms_between <- ss_between / (n_clusters - 1L)
  ms_within <- ss_within / (n_observations - n_clusters)

  # Effective cluster size for a one-way random-effects ANOVA with unequal n.
  n_effective <- (n_observations -
                    sum(cluster_sizes^2) / n_observations) /
    (n_clusters - 1L)
  denominator <- ms_between + (n_effective - 1) * ms_within

  if (!is.finite(n_effective) || n_effective <= 1 ||
        !is.finite(denominator) || denominator <= 0) {
    return(NA_real_)
  }

  (ms_between - ms_within) / denominator
}
