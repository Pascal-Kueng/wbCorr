check_assumptions <- function(col, name, method) {

  if (!is.numeric(col)) {
    col <- as.factor(col)
    col <- droplevels(col)
  }


  if (is.numeric(col)) {
    return(list(col = col, type = 'numeric', warning = "None"))
  }
  if (is.factor(col)) {
    if (nlevels(col) == 2) {
      return(list(col = as.numeric(col), type = 'binary', warning = "None"))
    }
    if (is.ordered(col)) {
      return(list(col = as.numeric(col), type = 'ordinal', warning = "None"))
    } else {
      warning(paste("The factor", name, "coded as non-binary nominal variable. Can not meaningfully be centered. Consider dummy-coding for each level."))
      return(list(col = as.numeric(col), type = 'nominal', warning = paste(name, "nominal variable?")))
    }
  }
  warning(paste("Failed to convert", name, "to numeric. Check coding of the variable and assumptions!"))
}
