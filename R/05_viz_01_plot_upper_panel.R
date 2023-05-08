custom_upper_panel <- function(x, y,
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
  x <- x[-c(1,2,3)]
  y <- y[-c(1,2,3)]


  # Valid pairs.
  valid_pairs <- is.finite(x) & is.finite(y)
  x <- x[valid_pairs]
  y <- y[valid_pairs]

  # prepare Tile
  if (var(x) == 0 | var(y) == 0 | is.na(var(x)) | is.na(var(y))) {
    msg = "NA"
  } else {
    linear_regression <- lm(y ~ x, na.action = 'na.omit')
    coef_value <- coef(linear_regression)[2]
    coef_value <- sprintf("%.2f", coef_value)

    if (is_weighted) {
      stars <- ""
      } else {
      p_value <- summary(linear_regression)$coefficients[2, 4]
      stars <- ""
      if (p_value < 0.001) {
        stars <- "***"
      } else if (p_value < 0.01) {
        stars <- "**"
      } else if (p_value < 0.05) {
        stars <- "*"
      }
    }


    if (standardize) {
      msg <- paste0("\u03B2 = ", coef_value, stars)
    } else {
      msg <- paste("b = ", coef_value, stars)
    }
  }

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, msg, ...)
}
