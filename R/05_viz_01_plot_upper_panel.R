custom_upper_panel <- function(x, y,
                               method,
                               auto_type,
                               var_type,
                               wbCorrObject,
                               df,
                               standardize,
                               plot_NA,
                               ...) {

  # Find out variable names
  x_idx <- round(x[1] * 100)
  y_idx <- round(y[1] * 100)

  x_name <- colnames(df)[x_idx]
  y_name <- colnames(df)[y_idx]

  # Find out type code
  x_type <- decode_type(x[3])
  y_type <- decode_type(y[3])

  # Show the correlation stored in the wbCorr object, so plot and tables agree.
  coefficient <- wbCorrObject$correlations[x_name, y_name]
  if (is.na(coefficient)) {
    msg <- "NA"
  } else {
    is_spearman <- method == 'spearman' || (auto_type && 'ordinal' %in% c(x_type, y_type))
    label <- if (is_spearman) "\u03C1 = " else "r = "
    stars <- p_value_to_asterisks(wbCorrObject$p_values[x_name, y_name])
    msg <- paste0(label, sprintf("%.2f", coefficient), stars)
  }

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, msg, ...)
}
