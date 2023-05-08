
encode_type <- function(df, types) {

  if (is.null(types)) {
    return(df)
  }

  num_types <- numeric(length = length(types))
  for (i in 1:length(types)) {
    type <- types[i]
    if (type == 'numeric') {
      num_type <- 1
    } else if (type == 'binary') {
      num_type <- 0
    } else if (type == 'ordinal') {
      num_type <- 2
    } else if (type == 'nominal') {
      num_type <- 3
    }
    num_types[i] <- num_type
  }

  return(rbind(num_types, df))
}


apply_types <- function(var, types) {

  if (is.null(types)) {
    return(df)
  }

  for (i in 1:ncol(df)) {
    type <- types[i]

    if (type == 'numeric') {
      next
    } else if (type == 'binary' | type == 'nominal') {
      df[i] <- as.factor(df[i])
    } else if (type == 'ordinal') {
      df[i] <- as.ordered(df[i])
    }
  }
  return(df)
}
