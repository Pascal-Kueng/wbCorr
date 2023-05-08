custom_lower_panel <- function(x, y, type = 'p',
                               auto_type, var_type,
                               outlier_detection,
                               outlier_threshold,
                               pch, dot_lwd,
                               reg_lwd,
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


  # Prepare Tile

  # Plot all points
  points(x, y, type = type,
         pch = pch, lwd = dot_lwd,
         col = "black", ...)

  # Create Abline (regression)
  linear_regression <- NULL

  tryCatch({
    linear_regression <- lm(y ~ x, na.action = 'na.omit')
  }, error = function(e) {})
  if (!is.null(linear_regression) && all(is.finite(coef(linear_regression)))) {
    a <-
      abline(linear_regression,
             col = "darkblue",
             lwd = reg_lwd)
  }


  # Identify Outliers
  if (outlier_detection == FALSE) {
    return(NULL)
  }
  x_outliers <- wb_check_outliers(x, outlier_detection, outlier_threshold)
  y_outliers <- wb_check_outliers(y, outlier_detection, outlier_threshold)

  # Plot x outliers in red

  if (length(x_outliers) > 0) {
    x_outlier_indices <- which(x %in% x_outliers)
    y_corresponding <- y[x_outlier_indices]
    if (length(x_outliers) == length(y_corresponding)) {
      points(x_outliers,
             y[x_outlier_indices],
             type = 'p',
             pch = pch,
             lwd = dot_lwd,
             col = "red")
    }
  }

  # Plot y outliers in red
  if (length(y_outliers) > 0) {
    y_outlier_indices <- which(y %in% y_outliers)
    x_corresponding <- x[y_outlier_indices]
    if (length(y_outliers) == length(x_corresponding)) {
      points(x[y_outlier_indices],
             y_outliers,
             type = 'p',
             pch = pch,
             lwd = dot_lwd,
             col = "red")
    }
  }
}
