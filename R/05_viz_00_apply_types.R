
encode_type <- function(df, types) {
  num_types <- numeric(length = ncol(df))
  for (i in 1:ncol(df)) {
    num_type <- 0.01
    if (is.null(types)) {
      num_type <- -.01
    } else {
      variable_name <- colnames(df)[i]
      type <- NULL
      if (!is.null(names(types)) && variable_name %in% names(types)) {
        type <- types[[variable_name]]
      } else if (length(types) >= i) {
        type <- types[[i]]
      }
      if (is.null(type) || length(type) != 1L || is.na(type)) {
        num_type <- 0.01
      } else if (type == 'numeric') {
        num_type <- 0.01
      } else if (type == 'binary') {
        num_type <- 0
      } else if (type == 'ordinal') {
        num_type <- 0.02
      } else if (type == 'nominal') {
        num_type <- 0.03
      }
    }
    num_types[i] <- num_type
  }
  return(rbind(num_types, -num_types, df))
}


decode_type <- function(num_type) {
  if (num_type == -0.01) {
    return(NULL)
  } else if (num_type == 0.01) {
    return('numeric')
  } else if (num_type == 0) {
    return('binary')
  } else if (num_type == 0.02) {
    return('ordinal')
  } else if (num_type == 0.03) {
    return('nominal')
  }
}


plot_pair_key <- function(x_name, y_name) {
  paste0(x_name, "\r", y_name)
}


panel_variable_name <- function(panel_values, df) {
  index <- as.integer(round(panel_values[1] * 100))
  if (is.na(index) || index < 1L || index > ncol(df)) {
    stop("Could not resolve a variable name for the plot panel.",
         call. = FALSE)
  }
  colnames(df)[index]
}


panel_pair_data <- function(x, y, df, plot_pairs = NULL) {
  x_name <- panel_variable_name(x, df)
  y_name <- panel_variable_name(y, df)

  if (!is.null(plot_pairs)) {
    pair <- plot_pairs[[plot_pair_key(x_name, y_name)]]
    if (is.null(pair)) {
      stop(sprintf("No prepared plot data are available for '%s' and '%s'.",
                   x_name, y_name),
           call. = FALSE)
    }
    return(c(list(x_name = x_name, y_name = y_name), pair))
  }

  # Compatibility path for direct calls to the historical panel helpers.
  x_values <- x[-c(1, 2, 3, 4)]
  y_values <- y[-c(1, 2, 3, 4)]
  valid <- is.finite(x_values) & is.finite(y_values)
  list(x_name = x_name,
       y_name = y_name,
       x = x_values[valid],
       y = y_values[valid],
       weights = NULL)
}
