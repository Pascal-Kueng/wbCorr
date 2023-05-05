wb_check_outliers <- function(data) {
  # Calculate the first quartile (Q1), third quartile (Q3), and IQR
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Identify outliers
  outliers <- data[data < lower_bound | data > upper_bound]

  return(outliers)
}

