
wb_plot <- function(x, y, which = NULL,
                    type = "p",
                    outlier_detection = 'zscore',
                    outlier_threshold = 'recommended',
                    ...) {
  if (is.null(which)) {
    which <- y
  }

  message("This may take a while...")
  if ('w' %in% which | 'within' %in% which) {
    pairs(x@within_df,
          main = "Bivariate associations of within-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type, outlier_detection, outlier_threshold, ...),
          ...)
  }
  if ('b' %in% which | 'between' %in% which) {
    pairs(x@between_df,
          main = "Bivariate associations of between-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type, outlier_detection, outlier_threshold, ...),
          ...)
  }
}
