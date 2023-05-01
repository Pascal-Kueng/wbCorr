check_assumptions <- function(col, name, method) {

  if (!is.numeric(col)) {
    col <- as.factor(col)
    col <- droplevels(col)
  }


  if (is.numeric(col)) {
    return(list(col = col, type = 'numeric', warning = NULL))
  }
  if (is.factor(col)) {
    if (nlevels(col) == 2 | is.ordered(col)) {
      return(list(col = as.numeric(col), type = 'factor', warning = NULL))
    } else {
      warning(paste("The factor", name, "has more than two levels and is unordered. Check coding of the variable and assumptions!"))
      return(list(col = as.numeric(col), type = 'factor', warning = "Unordered factor. Check coding and assumptions!"))
    }
  }
  warning(paste("Failed to convert", name, "to factor. Check coding of the variable and assumptions!"))
  return(list(col = col, type = NULL, warning = "Failed to convert. Check coding and assumptions!"))
}
