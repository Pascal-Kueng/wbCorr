
#######################################################
# Centering the data
#######################################################

# This function centers the data within and between clusters.
wbCenter <- function(input_data, cluster) {
  # Checks
  if (!is.data.frame(input_data)) {
    stop("input_data must be a data frame")
  } else if (ncol(input_data) < 2) {
    stop("input_data must have at least two columns")
  }

  # Determine Cluster Variable
  if (is.character(cluster)) {
    if (!cluster %in% colnames(input_data)) {
      stop("cluster must be a character (name of column in passed DF) or a numeric vector")
    }
    cluster_var <- input_data[[cluster]]
    input_data[[cluster]] <- NULL
  } else {
    cluster_var <- cluster
    cluster <- "cluster"
  }

  # Center the data
  df_within <- data.frame(cluster = cluster_var)
  df_between <- data.frame(cluster = cluster_var)

  for (name in colnames(input_data)) {
    col <- input_data[[name]]

    # Drop all non-numeric columns
    if(!is.numeric(col)) {
      warning(paste("CAUTION: Non-Numeric Variable", name ,"was set to NA!
      For correlations involving factors consider psych::statsBy"))
      col <- NA
    }

    grand_mean <- mean(col, na.rm = TRUE)
    col_grand_mean_c <- col - grand_mean

    # Centering between clusters
    between <- suppressWarnings(ave(col_grand_mean_c, cluster_var, FUN = function(x) mean(x, na.rm = TRUE)))
    df_between[[name]] <- between

    # Centering within clusters
    within <- col_grand_mean_c - between
    df_within[[name]] <- within

    # When there was an NA we set NA in the between centered Variable too to make sure we have a weighted score in the end.
    df_between[is.na(df_within[name]), name] <- NA
  }

  return(list(between = df_between, within = df_within))
}
