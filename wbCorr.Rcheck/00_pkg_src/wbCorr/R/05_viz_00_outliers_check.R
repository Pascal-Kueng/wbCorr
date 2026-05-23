
wb_check_outliers <- function(data, outlier_detection, outlier_threshold) {
  if (outlier_threshold == 'recommended' & outlier_detection == 'tukey') {
    outlier_threshold <- 1.5
  } else if (outlier_threshold == 'recommended') {
    outlier_threshold <- 3
  }

  if (outlier_detection == 'tukey') {
    return(wb_check_outliers_tukeys_fence(data, outlier_threshold))
  } else if (outlier_detection == 'zscore') {
    return(wb_check_outliers_zscore(data, outlier_threshold))
  } else if (outlier_detection == 'mad') {
    return(wb_check_outliers_mad(data, outlier_threshold))
  }
}


wb_check_outliers_tukeys_fence <- function(data, outlier_threshold) {
  # Calculate the first quartile (Q1), third quartile (Q3), and IQR
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - outlier_threshold * IQR
  upper_bound <- Q3 + outlier_threshold * IQR

  # Identify outliers
  outliers <- data[data < lower_bound | data > upper_bound]

  return(outliers)
}


wb_check_outliers_zscore <- function(data, outlier_threshold) {
  z_scores <- abs((data - mean(data, na.rm = TRUE)) / sd(data, na.rm = TRUE))
  outliers <- data[z_scores > outlier_threshold]
  return(outliers)
}


wb_check_outliers_mad <- function(data, outlier_threshold) {
  median_value <- median(data, na.rm = TRUE)
  deviations <- abs(data - median_value)
  mad_value <- median(deviations, na.rm = TRUE)
  outliers <- data[deviations > outlier_threshold * mad_value]
  return(outliers)
}
