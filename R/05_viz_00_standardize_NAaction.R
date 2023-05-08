

stdize <- function(var) {

  var <- scale(var)
  var <- scale(var)

  # make sure we only have valid numbers or NA
  var[!is.finite(var)] <- NA

  if (all(is.na(var))) {
    var <- 0
  }

  return(var)
}


