


input_validation_and_prep <- function(input_data, cluster, method) {
  # Input validation and error handling

  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  }
  if (!method %in% c("pearson", "spearman", "auto")) {
    stop("Invalid correlation method. Choose one of: 'pearson', 'spearman', and 'auto'.")
  }

  # Determine Cluster Variable
  if (is.null(cluster)) {
    stop("Cluster Variable empty!")
  }


  # Do we have a vector?
  if (length(cluster) > 1) {
    if (length(cluster) != nrow(input_data)) {
      stop("cluster must have one value per row of data.")
    }
    cluster_var <- as.factor(cluster)

    # is vector in data?
    for (name in colnames(input_data)) {
      if (identical(as.character(input_data[[name]]), as.character(cluster))) {
        input_data[[name]] <- NULL
      }
    }
  } else {
    # if we have a character string:
    if (!cluster %in% colnames(input_data)) {
      stop("cluster must be a character (name of column in passed DF) or a numeric vector. Name correct?")
    }

    cluster_var <- as.factor(input_data[[cluster]])
    input_data[[cluster]] <- NULL
  }

  # Rows without a cluster can not be centered.
  has_cluster <- !is.na(cluster_var)
  return(list(input_data = input_data[has_cluster, , drop = FALSE],
              cluster_var = droplevels(cluster_var[has_cluster])))
}
