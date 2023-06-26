#' @importFrom nlme lme VarCorr
#' @importFrom stats as.formula
compute_ICC1 <- function(input_data, cluster_var) {

  input_data$cluster_var <- cluster_var


  ICCs <- data.frame(variable = character(), ICC = numeric()) # Initialize an empty data frame


  # Replace spaces in variable names with underscores
  colnames(input_data) <- gsub("[^[:alnum:]]", "_", colnames(input_data))


  for (name in colnames(input_data)) {

    formula <- as.formula(paste0("`", name, "` ~ 1"))
    random_formula <- as.formula(paste0("~ 1 | cluster_var"))

    ICC <- 'NA'
    tryCatch({
      model <- suppressMessages(suppressWarnings(
        nlme::lme(fixed = formula, data = input_data, random = random_formula, na.action = 'na.omit')))

      vc <- VarCorr(model)
      between_variance <- as.numeric(vc[1,1]) # extract variance
      residual_variance <- as.numeric(vc[2,1]) # extract SD and square for var.

      ICC <- round(between_variance / (between_variance + residual_variance),2)
    }, error = function (e) {
      warning(paste("Could not compute ICC for variable", name, "due to singularity. Most likely it is 0 or 1."))
      ICC <- 'NA'
    })

    ICCs <- rbind(ICCs, data.frame(variable = name, ICC = ICC)) # Add the result to the data frame
  }

  return(ICCs)
}
