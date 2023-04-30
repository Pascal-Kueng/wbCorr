check_assumptions <- function(col, method) {
  if(!is.numeric(col)) {
    if(is.factor(col) & "is ordered for spearman, binary for spearman and pearson") {
      col <- as.numeric(col)
    } else {
      tryCatch({
        col <- as.numeric(col)
        warning("Converted non-numeric columns to numeric. Check assumptions!")
      }, error = function(e) {
        warning("CAUTION: Non-Numeric Variables are set to NA!")
        col <- NA
      })
    }
  }
  return(col)
}
