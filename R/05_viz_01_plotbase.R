wb_plot <- function(x, y, which = NULL, type = "p", ...) {
  if (is.null(which)) {
    which <- y
  }

  custom_panel <- function(x, y, type, ...) {
    points(x, y, type = type, ...)

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

  if ('w' %in% which | 'within' %in% which) {
    pairs(x@within_df,
          main = "Bivariate associations of within-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type, ...),
          ...)
  }
  if ('b' %in% which | 'between' %in% which) {
    pairs(x@between_df,
          main = "Bivariate associations of between-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type, ...),
          ...)
  }
}
