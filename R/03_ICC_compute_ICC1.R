

compute_ICC1 <- function(within_df, between_df_weighted) {

  ICCs <- data.frame(variable = character(), ICC = numeric()) # Initialize an empty data frame


  for (name in colnames(within_df)) {

    within_var <- var(within_df[[name]], na.rm = TRUE)
    between_var <- var(between_df_weighted[[name]], na.rm = TRUE)

    ICC <- between_var / (within_var + between_var)

    ICCs <- rbind(ICCs, data.frame(variable = name, ICC = ICC)) # Add the result to the data frame
  }

  return(ICCs)
}
