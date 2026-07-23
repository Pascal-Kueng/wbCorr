check_assumptions <- function(col, name, method) {
  if (is.numeric(col)) {
    non_finite <- is.infinite(col) | is.nan(col)
    warning_text <- "None"
    if (any(non_finite)) {
      col[non_finite] <- NA_real_
      warning_text <- paste(name, "non-finite values treated as missing")
      warning(sprintf("Variable '%s': Inf, -Inf, and NaN values were treated as missing.",
                      name),
              call. = FALSE)
    }

    observed_values <- unique(col[!is.na(col)])
    variable_type <- if (length(observed_values) == 2L) 'binary' else 'numeric'
    return(list(col = col, type = variable_type, warning = warning_text))
  }

  if (is.logical(col)) {
    return(list(col = as.numeric(col), type = 'binary', warning = "None"))
  }

  if (is.factor(col)) {
    declared_levels <- levels(col)
    if (length(declared_levels) == 2L) {
      encoded <- as.numeric(col) - 1
      warning(sprintf("Variable '%s' was encoded as 0/1 using factor level order: '%s' = 0, '%s' = 1.",
                      name, declared_levels[1], declared_levels[2]),
              call. = FALSE)
      return(list(col = encoded, type = 'binary', warning = "None"))
    }
    if (is.ordered(col)) {
      stop(sprintf("Variable '%s' is an ordered factor that does not declare exactly two levels. Convert it to explicit meaningful numeric scores before using wbCorr.",
                   name),
           call. = FALSE)
    }
    stop(sprintf("Variable '%s' is an unordered factor that does not declare exactly two levels. Declare an explicit binary level order or dummy-code it before using wbCorr.",
                 name),
         call. = FALSE)
  }

  if (is.character(col)) {
    stop(sprintf("Variable '%s' is character. Convert a binary character variable to a factor with explicit level order before using wbCorr.",
                 name),
         call. = FALSE)
  }

  stop(sprintf("Variable '%s' has unsupported class '%s'. Use numeric, logical, or a two-level factor.",
               name, paste(class(col), collapse = "/")),
       call. = FALSE)
}
