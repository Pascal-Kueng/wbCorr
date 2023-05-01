check_assumptions <- function(col, name, method) {
  # if column is numeric, all assumptions are met for pearson and spearman.

  if (!is.numeric(col)) {
    col <- as.factor(col)
    col <- droplevels(col)
  }

  if (method == 'pearson') {
    if (is.numeric(col)) {
      return(list(col = col, type = 'pearson', warning = NULL))
    }
    if (is.factor(col)) {
      if (nlevels(col) == 2){
        return(list(col = as.numeric(col), type = 'point-biserial', warning = NULL))
      } else {
        warning(paste("The factor", name, "has more than 2 levels and may not be suitable for point-biserial or pearson correlation. Check coding of the variable and assumptions."))
        return(list(col = as.numeric(col), type = 'pearson', warning = 'Factor with more than 2 levels - check coding and assumptions for method'))
      }
    }
    warning(paste0("Failed to convert variable ", name, ". Should be numeric or factor."))
    return(list(col = NULL, type = NULL, warning = 'Failed to convert to factor'))
  }

  if (method == 'spearman' | method == 'spearman-jackknife') {
    if(is.numeric(col)) {
      return(list(col = col, type = 'spearman', warning = NULL))
    }
    if(is.factor(col)) {
      if(nlevels(col) == 2) {
        return(list(col = col, type = 'spearman', warning = NULL))
      }
      if (is.ordered(col)) {
        return(list(col = col, type = 'spearman', warning = NULL))
      } else {
        warning(paste("The factor", name, "has more than two levels and is unordered. It may not suitable for spearman correlations. Check coding of the variable and assumptions!"))
        return(list(col = col, type = 'spearman', warning = "Unordered factor. Check coding and assumptions"))
      }
    }
    warning(paste0("Failed to convert variable", name, ". Should be numeric or factor."))
    return(list(col = NULL, type = NULL, warning = 'Failed to convert to factor'))
  }
}

if (method = 'auto') {
  if (is.numeric(col)) {
    return(list(col = col, type = 'pearson', warning = NULL))
  }
  if (is.factor(col)) {
    if (nlevels(col) == 2) {
      return(list(col = as.numeric(col), type = 'point-biserial', warning = NULL))
    }
    if (is.ordered(col)) {
      return(list(col = as.numeric(col), type = 'spearman', warning = NULL))
    }
  }
}
