
#' @importFrom lme4 lmer VarCorr
compute_ICC1 <- function(input_data, cluster_var, cluster_name) {
  library('lme4')
  if (!(cluster_name %in% colnames(input_data))) {
    input_data[[cluster_name]] <- cluster_var
  }

  ICCs <- data.frame(variable = character(), ICC = numeric()) # Initialize an empty data frame

  for (name in colnames(input_data)) {
    if (name == cluster_name || !is.numeric(input_data[[name]])) { # Check if the variable is numeric
      next
    }
    formula <- paste0("`", name, "` ~ 1 + ( 1 | ", cluster_name, " )") # Add backticks around the variable name
    model <- suppressMessages(suppressWarnings(lmer(formula = formula, data = input_data)))

    vc <- VarCorr(model)
    between_variance <- vc[[cluster_name]][1,1] # extract variance
    residual_variance <- (attr(vc, "sc"))^2 # extract SD and square for var.

    ICC <- round(between_variance / (between_variance + residual_variance),2)

    ICCs <- rbind(ICCs, data.frame(variable = name, ICC = ICC)) # Add the result to the data frame
  }

  return(ICCs)
}
