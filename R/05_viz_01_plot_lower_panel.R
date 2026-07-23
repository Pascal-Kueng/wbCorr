custom_lower_panel <- function(x, y, type = 'p',
                               method,
                               auto_type, var_type,
                               outlier_detection,
                               outlier_threshold,
                               pch, dot_lwd,
                               reg_lwd,
                               df,
                               standardize,
                               plot_NA,
                               plot_pairs = NULL,
                               ...) {
  pair <- panel_pair_data(x, y, df, plot_pairs)
  x <- pair$x
  y <- pair$y
  weights <- pair$weights
  pair_method <- if (is.null(pair$method)) method else pair$method

  valid_pairs <- is.finite(x) & is.finite(y)
  if (!is.null(weights)) {
    valid_pairs <- valid_pairs & is.finite(weights) & weights > 0
    weights <- weights[valid_pairs]
  }
  x <- x[valid_pairs]
  y <- y[valid_pairs]


  # Prepare Tile

  # Plot all points
  if (length(x) > 0L) {
    points(x, y, type = type,
           pch = pch, lwd = dot_lwd,
           col = "black", ...)
  }

  # Create Abline (regression)
  linear_regression <- NULL

  if (pair_method != "spearman" && length(x) >= 2L &&
      is.finite(var(x)) && var(x) > 0 &&
      is.finite(var(y)) && var(y) > 0) {
    tryCatch({
      linear_regression <- if (is.null(weights)) {
        lm(y ~ x, na.action = 'na.omit')
      } else {
        lm(y ~ x, weights = weights, na.action = 'na.omit')
      }
    }, error = function(e) {})
  }
  if (!is.null(linear_regression) && all(is.finite(coef(linear_regression)))) {
    a <-
      abline(linear_regression,
             col = "darkblue",
             lwd = reg_lwd)
  }


  # Identify Outliers
  if (outlier_detection == FALSE) {
    return(invisible(list(x = x,
                          y = y,
                          weights = weights,
                          fit = linear_regression)))
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

  invisible(list(x = x,
                 y = y,
                 weights = weights,
                 fit = linear_regression))
}
