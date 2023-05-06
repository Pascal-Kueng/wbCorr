
wb_plot <- function(x, y, which = NULL,
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

  within_df <- x@within_df
  between_df <- x@between_df

  if (standardize) {
    within_df <- scale(within_df)
    between_df <- scale(between_df)

    # make sure we only have valid numers or NA
    within_df[!is.finite(within_df)] <- NA
    between_df[!is.finite(between_df)] <- NA
    print(within_df)
    print(between_df)

    # Remove columns with only NA values
    within_df <- within_df[, colSums(is.na(within_df)) != nrow(within_df)]
    between_df <- between_df[, colSums(is.na(between_df)) != nrow(between_df)]
  }

  message("This may take a while...")
  if ('w' %in% which | 'within' %in% which) {
    pairs(within_df,
          main = "Bivariate associations of within-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type,
                                                   outlier_detection,
                                                   outlier_threshold,
                                                   pch, dot_lwd,
                                                   reg_lwd,
                                                   ...),
          ...)
  } else if ('b' %in% which | 'between' %in% which) {
    pairs(between_df,
          main = "Bivariate associations of between-cluster centered variables.",
          panel = function(x, y, ...) custom_panel(x, y, type,
                                                   outlier_detection,
                                                   outlier_threshold,
                                                   pch, dot_lwd,
                                                   reg_lwd,
                                                   ...),
          ...)
  }
}
