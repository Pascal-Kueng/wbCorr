custom_upper_panel <- function(x, y, ...) {
  valid_pairs <- is.finite(x) & is.finite(y)
  x <- x[valid_pairs]
  y <- y[valid_pairs]
  linear_regression <- lm(y ~ x, na.action = 'na.omit')
  coef_value <- coef(linear_regression)[2]
  text(mean(x), mean(y), sprintf("%.2f", coef_value), ...)
}
