custom_panel <- function(x, y, type, ...) {

  # Identify Outliers
  x_outliers <- wb_check_outliers(x)
  y_outliers <- wb_check_outliers(y)

  # Plot all points
  points(x, y, type = type, pch = 19, col = "black", ...)




  # Create Abline (regression)
  linear_regression <- NULL

  tryCatch({
    linear_regression <- lm(y ~ x, na.action = 'na.omit')
  }, error = function(e) {})
  if (!is.null(linear_regression) && all(is.finite(coef(linear_regression)))) {
    a <-
      abline(linear_regression,
             col = "blue",
             lwd = 2)
  }
}
