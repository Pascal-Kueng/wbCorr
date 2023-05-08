
#' @importFrom graphics abline pairs par points text
#' @importFrom stats coef lm median sd
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

  # Get the data
  within_df <- wbCorrObject@centered_data$within_df
  between_df <- wbCorrObject@centered_data$between_df

  if (standardize) {
    within_df <- scale(within_df)
    between_df <- scale(between_df)

    # make sure we only have valid numbers or NA
    within_df[is.infinite(within_df)] <- 0
    between_df[is.infinite(between_df)] <- 0
    within_df[is.nan(within_df)] <- 0
    between_df[is.nan(between_df)] <- 0
  }

  # extract settings
  is_weighted <- wbCorrObject@settings$weighted_between_statistics
  method <- wbCorrObject@settings$method
  types <- wbCorrObject@settings$auto_type

  # encode type into df
  within_df <- encode_type(within_df, types)
  between_df <- encode_type(between_df, types)

  # store variable index at top of df
  w_val <- 1:ncol(within_df) /1000
  w_val_comp <- -w_val # to compensate and make scaling equal on both sides.

  b_val <- 1:ncol(between_df) /1000
  b_val_comp <- - b_val

  within_df <- rbind(w_val, w_val_comp, within_df)
  between_df <- rbind(b_val, b_val_comp, between_df)

  # Remove columns with zero variance
  if (!plot_NA) {
    within_df <- within_df[, apply(within_df[-c(1,2), ], 2, function(x) var(x, na.rm = TRUE)) != 0]
    between_df <- between_df[, apply(between_df[-c(1,2), ], 2, function(x) var(x, na.rm = TRUE)) != 0]

  }

  message("This may take a while...")
  if ('w' %in% which | 'within' %in% which) {
    pairs(within_df,
          main = "Bivariate associations of within-cluster centered variables.",
          lower.panel = function(x, y, ...) custom_lower_panel(x, y,
                                                               type = type,
                                                               outlier_detection = outlier_detection,
                                                               outlier_threshold = outlier_threshold,
                                                               pch = pch, dot_lwd = dot_lwd,
                                                               reg_lwd = reg_lwd,
                                                               df = within_df,
                                                               standardize = standardize,
                                                               plot_NA = plot_NA,
                                                               ...),
          upper.panel = function(x, y, ...) custom_upper_panel(x, y,
                                                               wbCorrObject = wbCorrObject@within,
                                                               is_weighted = is_weighted,
                                                               df = within_df,
                                                               standardize = standardize,
                                                               plot_NA = plot_NA,
                                                               ...),
          ...)

  } else if ('b' %in% which | 'between' %in% which) {
    if (is_weighted) {
      message("No significance codes for between- regression coefficients can be calculated if weighted_between_statistics == TRUE.")
    }
      pairs(between_df,
          main = "Bivariate associations of between-cluster centered variables.",
          lower.panel = function(x, y, ...) custom_lower_panel(x, y,
                                                               type = type,
                                                               outlier_detection = outlier_detection,
                                                               outlier_threshold = outlier_threshold,
                                                               pch = pch, dot_lwd = dot_lwd,
                                                               reg_lwd = reg_lwd,
                                                               df = within_df,
                                                               standardize = standardize,
                                                               plot_NA = plot_NA,
                                                               ...),
          upper.panel = function(x, y, ...) custom_upper_panel(x, y,
                                                               wbCorrObject = wbCorrObject@within,
                                                               is_weighted = is_weighted,
                                                               df = within_df,
                                                               standardize = standardize,
                                                               plot_NA = plot_NA,
                                                               ...),
          ...)
  }
}
