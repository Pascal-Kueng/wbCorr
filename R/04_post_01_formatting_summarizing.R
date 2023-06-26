
#####################################
# Formatting and summarizing
#####################################

# Helper function to convert p-values to stars
p_value_to_asterisks <- function(p_value) {
  if (is.na(p_value)){
    return(NULL)
  } else if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return(NULL)
  }
}

# format the p-values in the matrix and add the stars
summarize_table <- function(p_values, correlations) {
  df_asterisks <- apply(p_values,
                        MARGIN = c(1, 2),
                        FUN = p_value_to_asterisks)

  df_summary <- mapply(function(x, y) paste0(sprintf("%.2f", x), y),  # <- Use sprintf to ensure 2 decimal places
                       as.matrix(correlations),
                       as.matrix(df_asterisks),
                       SIMPLIFY = FALSE)
  df_summary <- matrix(unlist(df_summary), nrow = nrow(correlations), byrow = TRUE)
  colnames(df_summary) <- colnames(correlations)
  rownames(df_summary) <- rownames(correlations)
  return(as.data.frame(df_summary))
}


# placing within- and between correlations above and below diagonal
combine_matrices <- function(within_matrix, between_matrix, ICC) {
  combined_matrix <- within_matrix

  for (i in 1:nrow(combined_matrix)) {
    for (j in 1:ncol(combined_matrix)) {
      if (i > j) {
        combined_matrix[i, j] <- between_matrix[i, j]
      }
    }
  }
  diag(combined_matrix) <- '[NA]'
  combined_matrix <- replace_with_ICC(combined_matrix, ICC)
  return(as.data.frame(combined_matrix))
}

# replacing diagonal with ICC
replace_with_ICC <- function(combined_matrix, ICCs) {
  # Place ICCs on the diagonal of the correlation matrix:
  for (i in 1:nrow(ICCs)) {
    var_name <- ICCs$variable[i]
    if (var_name %in% rownames(combined_matrix)) {
      combined_matrix[var_name, var_name] <- paste0('[',ICCs$ICC[i], ']')
    }
  }

  return(combined_matrix)
}
