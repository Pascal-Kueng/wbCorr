custom_upper_panel <- function(x, y, wbCorrObject, ...) {

  # print(wbCorrObject$correlations)

  valid_pairs <- is.finite(x) & is.finite(y)
  x <- x[valid_pairs]
  y <- y[valid_pairs]
  linear_regression <- lm(y ~ x, na.action = 'na.omit')
  coef_value <- coef(linear_regression)[2]

  usr_coords <- par("usr")
  x_middle <- (usr_coords[1] + usr_coords[2]) / 2
  y_middle <- (usr_coords[3] + usr_coords[4]) / 2

  text(x_middle, y_middle, sprintf("%.2f", coef_value), ...)
}
