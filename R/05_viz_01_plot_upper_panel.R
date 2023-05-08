custom_upper_panel <- function(x, y, wbCorrObject, standardize, ...) {

  valid_pairs <- is.finite(x) & is.finite(y)
  x <- x[valid_pairs]
  y <- y[valid_pairs]

  if (var(x) == 0 | var(y) == 0 | is.na(var(x)) | is.na(var(y))) {
    msg = "NA"
  } else {
    linear_regression <- lm(y ~ x, na.action = 'na.omit')
    coef_value <- coef(linear_regression)[2]
    coef_value <- sprintf("%.2f", coef_value)
    if (standardize) {
      msg <- paste("β =", coef_value)
    } else {
      msg <- paste("b =", coef_value)
    }
  }

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, msg, ...)
}