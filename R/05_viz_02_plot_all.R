
wb_plot <- function(x, y, which = NULL,
                    plot_NA = TRUE,
                    standardize = TRUE,
                    outlier_detection = 'zscore',
                    outlier_threshold = 'recommended',
                    type = "p",
                    pch = 20, dot_lwd = 2,
                    reg_lwd = 2,
                    ...) {
  if (is.null(which)) {
    which <- y
  }

  wbCorrObject <- x

  within_df <- wbCorrObject@within_df
  between_df <- wbCorrObject@between_df

  if (standardize) {
    within_df <- scale(within_df)
    between_df <- scale(between_df)

    # make sure we only have valid numbers or NA
    within_df[is.infinite(within_df)] <- 0
    between_df[is.infinite(between_df)] <- 0
    within_df[is.nan(within_df)] <- 0
    between_df[is.nan(between_df)] <- 0
  }

  # Remove columns with zero variance
  if (!plot_NA) {
    within_df <- within_df[, apply(within_df, 2, var) != 0]
    between_df <- between_df[, apply(between_df, 2, var) != 0]
  }

  message("This may take a while...")
  if ('w' %in% which | 'within' %in% which) {
    pairs(within_df,
          main = "Bivariate associations of within-cluster centered variables.",
          lower.panel = function(x, y, ...) custom_lower_panel(x, y, type,
                                                   outlier_detection,
                                                   outlier_threshold,
                                                   pch, dot_lwd,
                                                   reg_lwd,
                                                   ...),
          upper.panel = function(x, y, ...) custom_upper_panel(x, y,
                                                               wbCorrObject = wbCorrObject@within,
                                                               standardize = standardize,
                                                               ...),
          ...)
  } else if ('b' %in% which | 'between' %in% which) {
    pairs(between_df,
          main = "Bivariate associations of between-cluster centered variables.",
          lower.panel = function(x, y, ...) custom_lower_panel(x, y, type,
                                                         outlier_detection,
                                                         outlier_threshold,
                                                         pch, dot_lwd,
                                                         reg_lwd,
                                                         ...),
          upper.panel = function(x, y, ...) custom_upper_panel(x, y,
                                                               wbCorrObject = wbCorrObject@between,
                                                               standardize = standardize,
                                                               ...),
          ...)
  }
}
