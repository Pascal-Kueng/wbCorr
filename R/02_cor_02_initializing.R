
initializing_values <- function(input_data) {
  n_numeric <- ncol(input_data)
  p_matrix <- matrix(0,
                     ncol = n_numeric, nrow = n_numeric,
                     dimnames = list(names(input_data), names(input_data)))
  cor_matrix <- matrix(1,
                       ncol = n_numeric, nrow = n_numeric,
                       dimnames = list(names(input_data), names(input_data)))
  conf_int_df <- data.frame(Parameter1 = character(0),
                            Parameter2 = character(0),
                            CI_lower = numeric(0),
                            correlation_coefficient = numeric(0),
                            CI_upper = numeric(0))

  result_table <- data.frame(Parameter1 = numeric(0),
                             Parameter2 = numeric(0),
                             coefficient = numeric(0),
                             statistic = numeric(0),
                             df = numeric(0),
                             CI = character(0),
                             p = numeric(0))

  idx_combinations <- t(combn(n_numeric, 2))
  return(list(n_numeric = n_numeric,
              p_matrix = p_matrix,
              cor_matrix = cor_matrix,
              conf_int_df = conf_int_df,
              result_table = result_table,
              idx_combinations = idx_combinations))
}

