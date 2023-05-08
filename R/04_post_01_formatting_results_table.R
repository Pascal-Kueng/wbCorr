


format_result_table <- function(result_table, method, auto_type, var_type, confidence_level, bootstrap) {

  # For all
  ci_name <- paste0(confidence_level * 100, "% CI")
  colnames(result_table)[colnames(result_table) == "CI"] <- ci_name
  colnames(result_table)[colnames(result_table) == "statistic_type"] <- "statistic type"


  # rename columns and formatting main table: specific changes
  if (!auto_type) {
    if (method == 'pearson') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "pearson's r"
      if (bootstrap) {
        result_table$statistic <- NULL
      } else {
        colnames(result_table)[colnames(result_table) == "statistic"] <- "t-statistic"
      }
      result_table$method <- NULL
      result_table[['statistic type']] <- NULL
    } else if (method == 'spearman') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "spearman's rho"
      if (bootstrap) {
        result_table$statistic <- NULL
      } else {
        colnames(result_table)[colnames(result_table) == "statistic"] <- "z-statistic"
      }
      result_table$method <- NULL
      result_table[['statistic type']] <- NULL
    } else if (method == 'spearman-jackknife') {
      colnames(result_table)[colnames(result_table) == "coefficient"] <- "spearman's rho"
      result_table$method <- NULL
      result_table[['statistic type']] <- NULL
      result_table$statistic <- NULL
      result_table$df <- NULL
      result_table$p <- NULL
    }
  }


  # drop Warning column if there is none.

  if (all(result_table$warning == 'None')) {
    result_table$warning <- NULL
  }

  # Format p-value
  if (method == 'spearman-jackknife') {
    return(result_table)
  }

  result_table$p <- ifelse(result_table$p < .001, "< .001***",
                           ifelse(result_table$p < .01, sprintf("%.3f**", result_table$p),
                                  ifelse(result_table$p < .05, sprintf("%.3f*", result_table$p),
                                         sprintf("%.3f", result_table$p))))

  if (bootstrap) {
    result_table$df <- NULL
    result_table[['statistic type']] <- NULL
    colnames(result_table)[colnames(result_table) == ci_name] <- paste("BCa", ci_name)
    colnames(result_table)[colnames(result_table) == "p"] <- "BCa p"
    for (name in colnames(result_table)) {
      if (all(is.na(result_table[[name]]))) {
        result_table[[name]] <- NULL
      }
    }
  }


  return(result_table)
}
