

input_validation_and_prep <- function(input_data, cluster, method, weighted_between_statistics, bootstrap) {
  # Input validation and error handling

  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (!method %in% c("pearson", "spearman", "spearman-jackknife", "auto")) {
    stop("Invalid correlation method. Choose one of: 'pearson', 'spearman', and 'spearman-jackknife'.")
  }
  if (method == 'spearman-jackknife' & weighted_between_statistics == TRUE) {
    stop("weighted_between_statistics not supported for jackknife CIs.")
  }
  if (method == 'spearman-jackknife' & bootstrap == TRUE) {
    stop("Jackknife and bootstraping can't both be active at once.")
  }
  if (bootstrap == TRUE & weighted_between_statistics == TRUE) {
    stop("weighted between-statistics not supported with bootstraping.")
  }

  # Determine Cluster Variable
  if (is.null(cluster)) {
    stop("Cluster Variable empty!")
  }


  # Do we have a vector?
  if (length(cluster) > 1) {
    cluster_var <- as.factor(cluster)

    # is vector in data?
    for (name in colnames(input_data)) {
      if (all(as.numeric(as.factor(input_data[[name]])) == as.numeric(cluster_var), na.rm = TRUE)) {
        input_data[[name]] <- NULL
      }
    }
    return(cluster_var)
  }

  # if we have a character string:
  if (!cluster %in% colnames(input_data)) {
    stop("cluster must be a character (name of column in passed DF) or a numeric vector. Name correct?")
  }

  cluster_var <- as.factor(input_data[[cluster]])
  input_data[[cluster]] <- NULL
  return(cluster_var)
}
