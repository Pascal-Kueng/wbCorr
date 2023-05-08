
encode_type <- function(df, types) {

  num_types <- numeric(length = length(types))
  for (i in 1:ncol(df)) {
    if (is.null(types)) {
      num_type <- -.01
    } else {
      type <- types[i]
      if (type == 'numeric') {
        num_type <- 0.01
      } else if (type == 'binary') {
        num_type <- 0
      } else if (type == 'ordinal') {
        num_type <- 0.02
      } else if (type == 'nominal') {
        num_type <- 0.03
      }
      num_types[i] <- num_type
    }
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
