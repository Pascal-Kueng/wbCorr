custom_upper_panel <- function(x, y,
                               method,
                               auto_type,
                               var_type,
                               wbCorrObject,
                               is_weighted,
                               df,
                               standardize,
                               plot_NA,
                               plot_pairs = NULL,
                               ...) {
  pair <- panel_pair_data(x, y, df, plot_pairs)
  x_name <- pair$x_name
  y_name <- pair$y_name
  pair_method <- if (is.null(pair$method)) method else pair$method

  # The fitted object is authoritative for the annotation. Recomputing a
  # regression coefficient here can silently switch estimands when centering
  # is pair-specific or between-cluster means are weighted.
  coefficient <- wbCorrObject$correlations[x_name, y_name]
  if (length(coefficient) != 1L || !is.finite(coefficient)) {
    msg <- "NA"
  } else {
    p_value <- wbCorrObject$p_values[x_name, y_name]
    stars <- p_value_to_asterisks(p_value)
    coefficient_label <- if (pair_method == "spearman") "rho" else "r"
    msg <- paste0(coefficient_label, " = ",
                  sprintf("%.2f", coefficient), stars)
  }

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, msg, ...)
  invisible(msg)
}
