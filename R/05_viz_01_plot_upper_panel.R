custom_upper_panel <- function(x, y,
                               method,
                               auto_type,
                               var_type,
                               wbCorrObject,
                               is_weighted,
                               df,
                               standardize,
                               plot_NA,
                               ...) {

  # Find out variable names
  x_idx <- x[1] * 100
  y_idx <- y[1] * 100

  x_name <- colnames(df)[x_idx]
  y_name <- colnames(df)[y_idx]

  # Find out type code
  x_type <- decode_type(x[3])
  y_type <- decode_type(y[3])

  # remove coding from variables
  x <- x[-c(1,2,3,4)]
  y <- y[-c(1,2,3,4)]


  # Valid pairs.
  valid_pairs <- is.finite(x) & is.finite(y)
  x <- x[valid_pairs]
  y <- y[valid_pairs]

  # prepare Tile
  if (length(x) < 3L || var(x) == 0 | var(y) == 0 | is.na(var(x)) | is.na(var(y))) {
    msg = "NA"
  } else {
    p_value <- wbCorrObject$p_values[x_name, y_name]
    stars <- p_value_to_asterisks(p_value)

    if (method == "spearman") {
      coefficient <- wbCorrObject$correlations[x_name, y_name]
      msg <- paste0("rho = ", sprintf("%.2f", coefficient), stars)
    } else {
      linear_regression <- lm(y ~ x, na.action = 'na.omit')
      coefficient <- coef(linear_regression)[2]
      coefficient <- sprintf("%.2f", coefficient)
      if (standardize) {
        msg <- paste0("beta = ", coefficient, stars)
      } else {
        msg <- paste0("b = ", coefficient, stars)
      }
    }
  }

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, msg, ...)
  invisible(msg)
}
