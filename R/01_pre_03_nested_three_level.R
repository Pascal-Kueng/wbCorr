# Statistical core for a strictly nested three-level decomposition.
#
# The helpers remain self-contained so that their estimands can be tested
# directly as well as through the public wbCorrNested object.


validate_nested_three_level_hierarchy <- function(data, hierarchy) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }
  if (!is.list(hierarchy) ||
      !is.null(dim(hierarchy)) ||
      length(hierarchy) != 2L) {
    stop(
      paste0(
        "For three-level data, the cluster variable must be a fully named ",
        "list of exactly two column names, ordered lower-to-higher."
      ),
      call. = FALSE
    )
  }

  level_names <- names(hierarchy)
  if (is.null(level_names) ||
      anyNA(level_names) ||
      any(!nzchar(level_names)) ||
      anyDuplicated(level_names)) {
    stop(
      paste0(
        "The cluster hierarchy must be a fully named list with two unique, ",
        "non-empty level names."
      ),
      call. = FALSE
    )
  }

  column_names <- character(2L)
  for (index in seq_len(2L)) {
    column <- hierarchy[[index]]
    if (!is.character(column) ||
        length(column) != 1L ||
        is.na(column) ||
        !nzchar(column)) {
      stop(
        "Each hierarchy entry must be one non-missing, non-empty character column name.",
        call. = FALSE
      )
    }
    column_names[[index]] <- column
  }

  if (anyDuplicated(column_names)) {
    stop("The lower- and upper-level hierarchy columns must be different.",
         call. = FALSE)
  }
  if (anyDuplicated(colnames(data))) {
    stop("data must have unique column names.", call. = FALSE)
  }
  missing_columns <- setdiff(column_names, colnames(data))
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "Hierarchy column%s not found in data: %s.",
        if (length(missing_columns) == 1L) "" else "s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  identifiers <- lapply(column_names, function(column) data[[column]])
  for (index in seq_along(identifiers)) {
    identifier <- identifiers[[index]]
    if (!is.atomic(identifier) ||
        !is.null(dim(identifier)) ||
        length(identifier) != nrow(data)) {
      stop("Hierarchy columns must be atomic vectors with one value per data row.",
           call. = FALSE)
    }
    if (is.numeric(identifier) &&
        any(is.infinite(identifier) | is.nan(identifier))) {
      stop("Numeric hierarchy identifiers may contain NA but not Inf, -Inf, or NaN.",
           call. = FALSE)
    }
  }

  lower_missing <- is.na(identifiers[[1L]])
  upper_missing <- is.na(identifiers[[2L]])
  if (any(xor(lower_missing, upper_missing))) {
    stop(
      paste0(
        "A partially missing hierarchy path is not allowed: rows must contain ",
        "either a complete lower-to-upper path or two missing hierarchy ",
        "identifiers."
      ),
      call. = FALSE
    )
  }
  if (all(lower_missing)) {
    stop("At least one complete hierarchy path is required.", call. = FALSE)
  }

  complete_paths <- !lower_missing
  lower_values <- identifiers[[1L]][complete_paths]
  upper_values <- identifiers[[2L]][complete_paths]
  hierarchy_mapping <- unique(data.frame(
    lower = lower_values,
    upper = upper_values
  ))
  if (anyDuplicated(hierarchy_mapping$lower)) {
    stop(
      paste0(
        "The hierarchy must be strictly nested: every lower-level ID must ",
        "map to exactly one upper-level ID. Create an explicit globally ",
        "unique composite lower ID before calling wbCorr."
      ),
      call. = FALSE
    )
  }

  list(
    lower_level = level_names[[1L]],
    upper_level = level_names[[2L]],
    lower_column = column_names[[1L]],
    upper_column = column_names[[2L]]
  )
}


prepare_nested_three_level_data <- function(data,
                                            hierarchy,
                                            variables = NULL,
                                            hierarchy_info = NULL) {
  if (is.null(hierarchy_info)) {
    hierarchy_info <- validate_nested_three_level_hierarchy(data, hierarchy)
  }
  hierarchy_columns <- c(
    hierarchy_info$lower_column,
    hierarchy_info$upper_column
  )

  if (is.null(variables)) {
    variables <- setdiff(colnames(data), hierarchy_columns)
  }
  if (!is.character(variables) ||
      length(variables) < 2L ||
      anyNA(variables) ||
      any(!nzchar(variables)) ||
      anyDuplicated(variables)) {
    stop(
      "variables must contain at least two unique, non-missing analysis column names.",
      call. = FALSE
    )
  }
  missing_variables <- setdiff(variables, colnames(data))
  if (length(missing_variables) > 0L) {
    stop(
      sprintf(
        "Analysis column%s not found in data: %s.",
        if (length(missing_variables) == 1L) "" else "s",
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (any(variables %in% hierarchy_columns)) {
    stop("Hierarchy columns cannot also be analysis variables.", call. = FALSE)
  }

  analysis_data <- data.frame(row.names = seq_len(nrow(data)))
  var_type <- list()
  warnings <- list()
  for (variable in variables) {
    assumptions <- check_assumptions(
      data[[variable]],
      variable,
      method = "pearson"
    )
    analysis_data[[variable]] <- assumptions$col
    var_type[[variable]] <- assumptions$type
    warnings[[variable]] <- assumptions$warning
  }

  output <- list(
    analysis_data = analysis_data,
    lower_id = data[[hierarchy_info$lower_column]],
    upper_id = data[[hierarchy_info$upper_column]],
    hierarchy = hierarchy_info,
    var_type = var_type,
    warnings = warnings
  )
  class(output) <- c("wbCorr_three_level_prepared", "list")
  output
}


nested_three_level_unit_keys <- function(lower_id, upper_id) {
  if (length(lower_id) != length(upper_id)) {
    stop("lower_id and upper_id must have equal lengths.", call. = FALSE)
  }
  if (anyNA(lower_id) || anyNA(upper_id)) {
    stop("Unit keys require complete hierarchy identifiers.", call. = FALSE)
  }

  hierarchy_mapping <- unique(data.frame(
    lower = lower_id,
    upper = upper_id
  ))
  if (anyDuplicated(hierarchy_mapping$lower)) {
    stop(
      paste0(
        "The hierarchy must be strictly nested: every lower-level ID must ",
        "map to exactly one upper-level ID. Create an explicit globally ",
        "unique composite lower ID before calling wbCorr."
      ),
      call. = FALSE
    )
  }

  upper_values <- unique(upper_id)
  upper_key <- match(upper_id, upper_values)
  lower_values <- unique(lower_id)
  lower_key <- match(lower_id, lower_values)

  list(
    lower = lower_key,
    upper = upper_key,
    n_lower = length(lower_values),
    n_upper = length(upper_values)
  )
}


nested_three_level_empty_decomposition <- function(pair_rows = integer(0)) {
  empty_level1 <- data.frame(
    x = numeric(0),
    y = numeric(0),
    lower_unit = integer(0),
    upper_unit = integer(0)
  )
  empty_level2 <- data.frame(
    x = numeric(0),
    y = numeric(0),
    lower_unit = integer(0),
    upper_unit = integer(0)
  )
  empty_level3 <- data.frame(
    x = numeric(0),
    y = numeric(0),
    upper_unit = integer(0)
  )
  diagnostics <- data.frame(
    level = c("level1", "level2", "level3"),
    n_obs = 0L,
    n_units = 0L,
    n_level2 = 0L,
    n_level3 = 0L,
    n_lower_units = 0L,
    n_upper_units = 0L,
    n_informative_units = 0L,
    stringsAsFactors = FALSE
  )

  list(
    pair_rows = pair_rows,
    level1 = empty_level1,
    level2 = empty_level2,
    level3 = empty_level3,
    diagnostics = diagnostics
  )
}


decompose_nested_pair <- function(x, y, hierarchy_ids) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric vectors.", call. = FALSE)
  }
  if (!is.list(hierarchy_ids) || length(hierarchy_ids) < 1L) {
    stop(
      "hierarchy_ids must be a lower-to-higher list of grouping vectors.",
      call. = FALSE
    )
  }

  input_lengths <- c(length(x), length(y), lengths(hierarchy_ids))
  if (length(unique(input_lengths)) != 1L) {
    stop(
      "x, y, and every hierarchy identifier must have equal lengths.",
      call. = FALSE
    )
  }
  for (identifier in hierarchy_ids) {
    if (!is.atomic(identifier) || !is.null(dim(identifier))) {
      stop("Hierarchy identifiers must be atomic vectors.", call. = FALSE)
    }
  }

  complete_rows <- is.finite(x) & is.finite(y)
  for (identifier in hierarchy_ids) {
    complete_rows <- complete_rows & !is.na(identifier)
  }
  pair_rows <- which(complete_rows)
  n_hierarchy_levels <- length(hierarchy_ids)
  if (length(pair_rows) == 0L) {
    empty_components <- lapply(
      seq_len(n_hierarchy_levels + 1L),
      function(index) data.frame(x = numeric(0), y = numeric(0))
    )
    empty_keys <- lapply(
      seq_len(n_hierarchy_levels + 1L),
      function(index) vector("list", n_hierarchy_levels)
    )
    return(list(
      pair_rows = pair_rows,
      components = empty_components,
      unit_keys = empty_keys,
      n_groups = integer(n_hierarchy_levels),
      n_informative_units = integer(n_hierarchy_levels + 1L)
    ))
  }

  pair_x <- as.numeric(x[pair_rows])
  pair_y <- as.numeric(y[pair_rows])
  pair_ids <- lapply(hierarchy_ids, function(identifier) {
    identifier[pair_rows]
  })

  if (n_hierarchy_levels > 1L) {
    for (level in seq_len(n_hierarchy_levels - 1L)) {
      mapping <- unique(data.frame(
        child = pair_ids[[level]],
        parent = pair_ids[[level + 1L]],
        stringsAsFactors = FALSE
      ))
      if (anyDuplicated(mapping$child)) {
        stop(
          paste0(
            "The hierarchy must be strictly nested: every unit must map to ",
            "exactly one parent unit."
          ),
          call. = FALSE
        )
      }
    }
  }

  hierarchy_keys <- lapply(pair_ids, function(identifier) {
    match(identifier, unique(identifier))
  })
  n_groups <- vapply(
    hierarchy_keys,
    function(keys) length(unique(keys)),
    integer(1)
  )

  components <- vector("list", n_hierarchy_levels + 1L)
  unit_keys <- vector("list", n_hierarchy_levels + 1L)
  n_informative_units <- integer(n_hierarchy_levels + 1L)
  current_x <- pair_x
  current_y <- pair_y
  representative_rows <- seq_along(pair_x)

  for (level in seq_len(n_hierarchy_levels)) {
    current_group_keys <- hierarchy_keys[[level]][representative_rows]
    group_values <- unique(current_group_keys)
    group_index <- match(current_group_keys, group_values)
    rows_by_group <- split(
      seq_along(group_index),
      factor(group_index, levels = seq_along(group_values))
    )

    group_mean_x <- numeric(length(group_values))
    group_mean_y <- numeric(length(group_values))
    residual_x <- numeric(length(current_x))
    residual_y <- numeric(length(current_y))
    next_representative_rows <- integer(length(group_values))

    for (group in seq_along(rows_by_group)) {
      rows <- rows_by_group[[group]]
      group_mean_x[[group]] <- mean(current_x[rows])
      group_mean_y[[group]] <- mean(current_y[rows])
      residual_x[rows] <- current_x[rows] - group_mean_x[[group]]
      residual_y[rows] <- current_y[rows] - group_mean_y[[group]]
      next_representative_rows[[group]] <- representative_rows[rows[[1L]]]
    }

    components[[level]] <- data.frame(x = residual_x, y = residual_y)
    unit_keys[[level]] <- lapply(hierarchy_keys, function(keys) {
      keys[representative_rows]
    })
    n_informative_units[[level]] <- sum(
      lengths(rows_by_group) >= 2L
    )
    current_x <- group_mean_x
    current_y <- group_mean_y
    representative_rows <- next_representative_rows
  }

  highest_level <- n_hierarchy_levels + 1L
  components[[highest_level]] <- data.frame(x = current_x, y = current_y)
  unit_keys[[highest_level]] <- lapply(hierarchy_keys, function(keys) {
    keys[representative_rows]
  })
  n_informative_units[[highest_level]] <- length(current_x)

  list(
    pair_rows = pair_rows,
    components = components,
    unit_keys = unit_keys,
    n_groups = n_groups,
    n_informative_units = n_informative_units
  )
}


decompose_nested_three_level_pair <- function(x,
                                              y,
                                              lower_id,
                                              upper_id) {
  decomposition <- decompose_nested_pair(
    x,
    y,
    hierarchy_ids = list(lower_id, upper_id)
  )
  if (length(decomposition$pair_rows) == 0L) {
    return(nested_three_level_empty_decomposition(
      decomposition$pair_rows
    ))
  }

  n_obs <- length(decomposition$pair_rows)
  n_lower <- decomposition$n_groups[[1L]]
  n_upper <- decomposition$n_groups[[2L]]
  level1 <- data.frame(
    decomposition$components[[1L]],
    lower_unit = decomposition$unit_keys[[1L]][[1L]],
    upper_unit = decomposition$unit_keys[[1L]][[2L]]
  )
  level2 <- data.frame(
    decomposition$components[[2L]],
    lower_unit = decomposition$unit_keys[[2L]][[1L]],
    upper_unit = decomposition$unit_keys[[2L]][[2L]]
  )
  level3 <- data.frame(
    decomposition$components[[3L]],
    upper_unit = decomposition$unit_keys[[3L]][[2L]]
  )
  diagnostics <- data.frame(
    level = c("level1", "level2", "level3"),
    n_obs = rep.int(n_obs, 3L),
    n_units = c(n_obs, n_lower, n_upper),
    n_level2 = rep.int(n_lower, 3L),
    n_level3 = rep.int(n_upper, 3L),
    n_lower_units = rep.int(n_lower, 3L),
    n_upper_units = rep.int(n_upper, 3L),
    n_informative_units = decomposition$n_informative_units,
    stringsAsFactors = FALSE
  )

  list(
    pair_rows = decomposition$pair_rows,
    level1 = level1,
    level2 = level2,
    level3 = level3,
    diagnostics = diagnostics
  )
}


nested_three_level_pearson <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Prepared x and y vectors must have equal lengths.", call. = FALSE)
  }
  if (length(x) < 2L) {
    return(list(
      coefficient = NA_real_,
      status = "not_estimable",
      reason = "fewer_than_two_units"
    ))
  }
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    return(list(
      coefficient = NA_real_,
      status = "not_estimable",
      reason = "non_finite_prepared_values"
    ))
  }

  variance_x <- stats::var(x)
  variance_y <- stats::var(y)
  zero_x <- !is.finite(variance_x) || variance_x <= 0
  zero_y <- !is.finite(variance_y) || variance_y <= 0
  if (zero_x || zero_y) {
    reason <- if (zero_x && zero_y) {
      "zero_variance_both"
    } else if (zero_x) {
      "zero_variance_parameter1"
    } else {
      "zero_variance_parameter2"
    }
    return(list(
      coefficient = NA_real_,
      status = "not_estimable",
      reason = reason
    ))
  }

  coefficient <- suppressWarnings(stats::cor(x, y, method = "pearson"))
  if (!is.finite(coefficient)) {
    return(list(
      coefficient = NA_real_,
      status = "not_estimable",
      reason = "non_finite_correlation"
    ))
  }

  list(
    coefficient = unname(coefficient),
    status = "ok",
    reason = NA_character_
  )
}


nested_three_level_pair_results <- function(decomposition,
                                            parameter1,
                                            parameter2) {
  if (!is.character(parameter1) ||
      length(parameter1) != 1L ||
      is.na(parameter1) ||
      !is.character(parameter2) ||
      length(parameter2) != 1L ||
      is.na(parameter2)) {
    stop("parameter1 and parameter2 must be scalar character names.",
         call. = FALSE)
  }

  level_names <- c("level1", "level2", "level3")
  rows <- vector("list", length(level_names))
  coefficients <- setNames(rep(NA_real_, length(level_names)), level_names)

  for (index in seq_along(level_names)) {
    level <- level_names[[index]]
    values <- decomposition[[level]]
    result <- nested_three_level_pearson(values$x, values$y)
    diagnostic <- decomposition$diagnostics[
      decomposition$diagnostics$level == level,
      ,
      drop = FALSE
    ]
    coefficients[[level]] <- result$coefficient
    rows[[index]] <- data.frame(
      Parameter1 = parameter1,
      Parameter2 = parameter2,
      level = level,
      method = "pearson's r",
      coefficient = result$coefficient,
      n_obs = diagnostic$n_obs,
      n_units = diagnostic$n_units,
      n_level2 = diagnostic$n_level2,
      n_level3 = diagnostic$n_level3,
      n_lower_units = diagnostic$n_lower_units,
      n_upper_units = diagnostic$n_upper_units,
      n_informative_units = diagnostic$n_informative_units,
      status = result$status,
      reason = result$reason,
      stringsAsFactors = FALSE
    )
  }

  list(
    coefficients = coefficients,
    table = do.call(rbind, rows)
  )
}


compute_nested_three_level_correlations <- function(prepared) {
  if (!inherits(prepared, "wbCorr_three_level_prepared")) {
    stop(
      "prepared must be created by prepare_nested_three_level_data().",
      call. = FALSE
    )
  }

  analysis_data <- prepared$analysis_data
  variables <- colnames(analysis_data)
  if (length(variables) < 2L) {
    stop("At least two analysis variables are required.", call. = FALSE)
  }

  matrices <- lapply(c("level1", "level2", "level3"), function(level) {
    matrix(
      NA_real_,
      nrow = length(variables),
      ncol = length(variables),
      dimnames = list(variables, variables)
    )
  })
  names(matrices) <- c("level1", "level2", "level3")

  result_rows <- list()
  row_index <- 0L
  combinations <- utils::combn(seq_along(variables), 2L)
  for (combination in seq_len(ncol(combinations))) {
    index1 <- combinations[1L, combination]
    index2 <- combinations[2L, combination]
    parameter1 <- variables[[index1]]
    parameter2 <- variables[[index2]]
    decomposition <- decompose_nested_three_level_pair(
      analysis_data[[index1]],
      analysis_data[[index2]],
      prepared$lower_id,
      prepared$upper_id
    )
    pair_result <- nested_three_level_pair_results(
      decomposition,
      parameter1,
      parameter2
    )

    for (level in names(matrices)) {
      coefficient <- pair_result$coefficients[[level]]
      matrices[[level]][index1, index2] <- coefficient
      matrices[[level]][index2, index1] <- coefficient
    }
    row_index <- row_index + 1L
    result_rows[[row_index]] <- pair_result$table
  }

  # Diagonals describe estimability at each level, not an ICC.  A self-pair
  # follows exactly the same pair-specific hierarchy and centering rules.
  for (index in seq_along(variables)) {
    decomposition <- decompose_nested_three_level_pair(
      analysis_data[[index]],
      analysis_data[[index]],
      prepared$lower_id,
      prepared$upper_id
    )
    for (level in names(matrices)) {
      result <- nested_three_level_pearson(
        decomposition[[level]]$x,
        decomposition[[level]]$y
      )
      matrices[[level]][index, index] <-
        if (identical(result$status, "ok")) 1 else NA_real_
    }
  }

  output <- list(
    correlations = lapply(matrices, function(values) {
      as.data.frame(values, check.names = FALSE)
    }),
    table = do.call(rbind, result_rows),
    settings = list(
      hierarchy = prepared$hierarchy,
      method = "pearson",
      weighting = "equal_units",
      centering_rows = "pairwise_complete",
      inference = "none"
    )
  )
  class(output) <- c("wbCorr_three_level_core", "list")
  output
}


bootstrap_nested_three_level_pair <- function(x,
                                              y,
                                              lower_id,
                                              upper_id,
                                              nboot = 1000L,
                                              confidence_level = 0.95) {
  if (!is.numeric(nboot) ||
      length(nboot) != 1L ||
      is.na(nboot) ||
      !is.finite(nboot) ||
      nboot < 10 ||
      nboot > .Machine$integer.max ||
      nboot != floor(nboot)) {
    stop("nboot must be one finite whole number of at least 10.",
         call. = FALSE)
  }
  if (!is.numeric(confidence_level) ||
      length(confidence_level) != 1L ||
      is.na(confidence_level) ||
      !is.finite(confidence_level) ||
      confidence_level <= 0 ||
      confidence_level >= 1) {
    stop(
      "confidence_level must be one finite numeric value strictly between 0 and 1.",
      call. = FALSE
    )
  }

  observed_decomposition <- decompose_nested_three_level_pair(
    x,
    y,
    lower_id,
    upper_id
  )
  observed <- vapply(
    c("level1", "level2", "level3"),
    function(level) {
      nested_three_level_pearson(
        observed_decomposition[[level]]$x,
        observed_decomposition[[level]]$y
      )$coefficient
    },
    numeric(1)
  )

  n_upper <- observed_decomposition$diagnostics$n_upper_units[[1L]]
  if (n_upper < 3L) {
    return(data.frame(
      level = c("level1", "level2", "level3"),
      coefficient = unname(observed),
      CI_lower = NA_real_,
      CI_upper = NA_real_,
      n_boot_attempted = 0L,
      n_boot_valid = 0L,
      inference_status = "unavailable",
      inference_reason = "fewer_than_three_contributing_upper_units",
      stringsAsFactors = FALSE
    ))
  }

  # Sample every intact upper unit in the analysis frame, including units with
  # no complete x-y pair. Their missingness must remain part of a whole-cluster
  # bootstrap draw; the decomposition below decides whether a replicate is
  # estimable after sampling.
  hierarchy_rows <- !is.na(lower_id) & !is.na(upper_id)
  frame_x <- as.numeric(x[hierarchy_rows])
  frame_y <- as.numeric(y[hierarchy_rows])
  frame_lower <- lower_id[hierarchy_rows]
  frame_upper <- upper_id[hierarchy_rows]
  original_keys <- nested_three_level_unit_keys(frame_lower, frame_upper)
  upper_units <- seq_len(original_keys$n_upper)
  rows_by_upper <- split(seq_along(frame_x), original_keys$upper)
  bootstrap_values <- matrix(
    NA_real_,
    nrow = as.integer(nboot),
    ncol = 3L,
    dimnames = list(NULL, c("level1", "level2", "level3"))
  )

  for (bootstrap_index in seq_len(nboot)) {
    sampled_upper <- sample(
      upper_units,
      size = length(upper_units),
      replace = TRUE
    )
    bootstrap_x_parts <- vector("list", length(sampled_upper))
    bootstrap_y_parts <- vector("list", length(sampled_upper))
    bootstrap_lower_parts <- vector("list", length(sampled_upper))
    bootstrap_upper_parts <- vector("list", length(sampled_upper))

    for (draw_index in seq_along(sampled_upper)) {
      rows <- rows_by_upper[[sampled_upper[[draw_index]]]]
      bootstrap_x_parts[[draw_index]] <- frame_x[rows]
      bootstrap_y_parts[[draw_index]] <- frame_y[rows]
      # The draw index makes every sampled copy a new independent top-level
      # cluster.  Combining it with the original lower-unit key preserves
      # all lower units without merging duplicated cluster draws.
      bootstrap_upper_parts[[draw_index]] <- rep.int(
        draw_index,
        length(rows)
      )
      bootstrap_lower_parts[[draw_index]] <- paste0(
        "bootstrap-draw-",
        draw_index,
        "-lower-",
        original_keys$lower[rows]
      )
    }
    bootstrap_x <- unlist(bootstrap_x_parts, use.names = FALSE)
    bootstrap_y <- unlist(bootstrap_y_parts, use.names = FALSE)
    bootstrap_lower <- unlist(bootstrap_lower_parts, use.names = FALSE)
    bootstrap_upper <- unlist(bootstrap_upper_parts, use.names = FALSE)

    decomposition <- decompose_nested_three_level_pair(
      bootstrap_x,
      bootstrap_y,
      bootstrap_lower,
      bootstrap_upper
    )
    for (level_index in seq_len(3L)) {
      level <- colnames(bootstrap_values)[[level_index]]
      bootstrap_values[bootstrap_index, level_index] <-
        nested_three_level_pearson(
          decomposition[[level]]$x,
          decomposition[[level]]$y
        )$coefficient
    }
  }

  alpha <- 1 - confidence_level
  output_rows <- vector("list", 3L)
  for (level_index in seq_len(3L)) {
    level <- colnames(bootstrap_values)[[level_index]]
    valid <- bootstrap_values[, level_index]
    valid <- valid[is.finite(valid)]
    n_valid <- length(valid)
    interval <- c(NA_real_, NA_real_)
    status <- "unavailable"
    reason <- "fewer_than_ten_valid_bootstrap_replicates"

    if (n_valid >= 10L) {
      interval <- as.numeric(stats::quantile(
        valid,
        probs = c(alpha / 2, 1 - alpha / 2),
        names = FALSE,
        na.rm = TRUE
      ))
      if (n_valid == nboot) {
        status <- "ok"
        reason <- NA_character_
      } else {
        status <- "partial"
        reason <- "invalid_bootstrap_replicates_excluded"
      }
    }

    output_rows[[level_index]] <- data.frame(
      level = level,
      coefficient = observed[[level]],
      CI_lower = interval[[1L]],
      CI_upper = interval[[2L]],
      n_boot_attempted = as.integer(nboot),
      n_boot_valid = as.integer(n_valid),
      inference_status = status,
      inference_reason = reason,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, output_rows)
}
