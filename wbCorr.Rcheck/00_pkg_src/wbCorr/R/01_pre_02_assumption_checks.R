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
      warning(paste("The factor", name, "coded as binary ordinal variable. Centering will make this a continuous variable if it varies on more than 1 level. Make sure to check the interpretation."))
      return(list(col = as.numeric(col), type = 'binary', warning = "None"))
    }
    if (is.ordered(col)) {
      warning(paste("The factor", name, "coded as non-binary ordinal variable. Centering may not be meaningful and correlations may not be interpretable. Consider dummy-coding for each level."))
      return(list(col = as.numeric(col), type = 'ordinal', warning = "ordinal variable?"))
    } else {
      warning(paste("The factor", name, "coded as non-binary nominal variable. Centering is likely not meaningful and correlations may not be interpretable. Consider dummy-coding for each level."))
      return(list(col = as.numeric(col), type = 'nominal', warning = paste(name, "nominal variable?")))
    }
  }
  return(list(col = NULL, type = NULL, warning = paste(name, "ERROR")))
  warning(paste("Failed to convert", name, "to numeric. Check coding of the variable and assumptions!"))
}
