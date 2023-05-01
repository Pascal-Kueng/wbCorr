


format_result_table <- function(result_table, method, auto_type) {


  # rename columns and formatting main table

  colnames(result_table)[colnames(result_table) == "CI"] <- "95% CI"
  colnames(result_table)[colnames(result_table) == "statistic_type"] <- "statistic type"

  if (is.null(auto_type)) {
    if (method == 'pearson') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "pearson's r"
      colnames(result_table)[colnames(result_table) == "statistic"] <- "t"
    } else if (method == 'spearman') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "spearman's rho"
      colnames(result_table)[colnames(result_table) == "statistic_type"] <- "statistic type"
      colnames(result_table)[colnames(result_table) == "statistic"] <- "z"
    } else if (method == 'spearman-jackknife') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "spearman's rho"
    }
  }


  # drop columns with no variation
  for (name in colnames(result_table)) {
    if (length(unique(result_table[[name]])) == 1) {
      result_table[[name]] <- NULL
    }
  }

  if (method == 'spearman-jackknife') {
    return(result_table)
  }
  result_table$p <- ifelse(result_table$p < .001, "< .001***",
                           ifelse(result_table$p < .01, sprintf("%.3f**", result_table$p),
                                  ifelse(result_table$p < .05, sprintf("%.3f*", result_table$p),
                                         sprintf("%.3f", result_table$p))))
  return(result_table)
}
