
#######################################################
# Centering the data
#######################################################

# This function centers the data within and between clusters.
#' @importFrom stats aggregate
wbCenter <- function(input_data, cluster, method, weighted_between_statistics = FALSE) {
  # Checks
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  } else if (ncol(input_data) < 2) {
    stop("input_data must have at least two columns")
  }

  # Determine Cluster Variable
  if (length(cluster) > 1) {
    cluster <- as.factor(cluster)
  }
  if (is.character(cluster)) {
    if (!cluster %in% colnames(input_data)) {
      stop("cluster must be a character (name of column in passed DF) or a numeric vector. Name correct?")
    }
    cluster_var <- as.factor(input_data[[cluster]])
    input_data[[cluster]] <- NULL
  } else {
    cluster_var <- as.factor(cluster)
    cluster <- "cluster"
  }

  # Center the data
  df_within <- data.frame(cluster = cluster_var)
  df_between <- data.frame(cluster = cluster_var)


  for (name in colnames(input_data)) {
    col <- input_data[[name]]

    # Check if we have variables that may violate assumptions.
    assumptions <- check_assumptions(col, name, method)
    col <- assumptions$col

    grand_mean <- mean(col, na.rm = TRUE)
    col_grand_mean_c <- col - grand_mean

    # Centering between clusters
    between <- suppressWarnings(ave(col_grand_mean_c, cluster_var, FUN = function(x) mean(x, na.rm = TRUE)))
    df_between[[name]] <- between

    # Centering within clusters
    within <- col_grand_mean_c - between
    df_within[[name]] <- within

    # When there was an NA we set NA in the between centered Variable too to make sure we have a weighted score in the end.
    if (weighted_between_statistics) {
      df_between[is.na(df_within[name]), name] <- NA
    }
  }

  if (weighted_between_statistics == FALSE) {
    df_between <- suppressWarnings(aggregate(df_between, by = list(cluster_var), FUN = mean))
    df_between$cluster <- df_between$Group.1
    df_between$Group.1 <- NULL
  }

  return(list(between = df_between, within = df_within))
}
