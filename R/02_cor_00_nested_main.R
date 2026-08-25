#######################################################
# Three-level wbCorr construction
#######################################################

wbCorr_three_level <- function(data,
                               cluster,
                               confidence_level,
                               method,
                               bootstrap,
                               nboot,
                               inference,
                               weighted_between_statistics,
                               between_weighting,
                               between_inference,
                               centering_rows,
                               missing_data,
                               inference_missing,
                               between_weighting_missing,
                               between_inference_missing,
                               centering_rows_missing,
                               missing_data_missing,
                               supplied_call) {
  hierarchy_info <- validate_wbcorr_inputs(
    data,
    cluster,
    confidence_level,
    method,
    bootstrap,
    nboot,
    weighted_between_statistics
  )
  nboot <- as.integer(nboot)

  legacy_bootstrap_requested <- isTRUE(bootstrap)
  if (legacy_bootstrap_requested && inference_missing) {
    inference <- "cluster_bootstrap"
    warning(
      "bootstrap = TRUE is deprecated; using inference = 'cluster_bootstrap'.",
      call. = FALSE
    )
  } else if (inference_missing) {
    # Analytic inference has not been validated for the three-level estimands.
    inference <- "none"
  } else {
    inference <- resolve_wbcorr_choice(
      inference,
      c("analytic", "none", "cluster_bootstrap"),
      "inference",
      FALSE
    )
  }
  if (legacy_bootstrap_requested &&
      !inference_missing &&
      inference != "cluster_bootstrap") {
    warning(
      paste0(
        "bootstrap = TRUE is deprecated and ignored because inference is ",
        "not 'cluster_bootstrap'."
      ),
      call. = FALSE
    )
  }

  if (!is.null(weighted_between_statistics) && between_weighting_missing) {
    between_weighting <- if (isTRUE(weighted_between_statistics)) {
      "cluster_size"
    } else {
      "equal_clusters"
    }
  }
  between_weighting <- resolve_wbcorr_choice(
    between_weighting,
    c("equal_clusters", "cluster_size"),
    "between_weighting",
    between_weighting_missing && is.null(weighted_between_statistics)
  )
  between_inference <- resolve_wbcorr_choice(
    between_inference,
    c("analytic", "none"),
    "between_inference",
    between_inference_missing
  )
  centering_rows <- resolve_wbcorr_choice(
    centering_rows,
    c("pairwise_complete", "all_available"),
    "centering_rows",
    centering_rows_missing
  )
  missing_data <- resolve_wbcorr_choice(
    missing_data,
    c("pairwise", "listwise"),
    "missing_data",
    missing_data_missing
  )

  if (method != "pearson") {
    stop(
      paste0(
        "Three-level decomposition currently supports Pearson correlations ",
        "only (method = 'pearson')."
      ),
      call. = FALSE
    )
  }
  if (inference == "analytic") {
    stop(
      paste0(
        "Analytic inference is not available for three-level decomposition. ",
        "Use inference = 'none' or 'cluster_bootstrap'."
      ),
      call. = FALSE
    )
  }
  if (between_weighting != "equal_clusters") {
    stop(
      paste0(
        "Three-level decomposition currently uses equal-unit weighting at ",
        "each level; between_weighting must be 'equal_clusters'."
      ),
      call. = FALSE
    )
  }
  if (centering_rows != "pairwise_complete") {
    stop(
      paste0(
        "Three-level decomposition currently supports only ",
        "centering_rows = 'pairwise_complete'."
      ),
      call. = FALSE
    )
  }

  prepared <- prepare_nested_three_level_data(
    data,
    cluster,
    hierarchy_info = hierarchy_info
  )
  if (missing_data == "listwise") {
    complete_rows <- complete.cases(
      prepared$analysis_data,
      prepared$lower_id,
      prepared$upper_id
    )
    prepared$analysis_data <- prepared$analysis_data[
      complete_rows,
      ,
      drop = FALSE
    ]
    prepared$lower_id <- prepared$lower_id[complete_rows]
    prepared$upper_id <- prepared$upper_id[complete_rows]
  }

  core <- compute_nested_three_level_correlations(prepared)
  bootstrap_results <- NULL
  if (inference == "cluster_bootstrap") {
    bootstrap_results <- compute_nested_three_level_bootstrap(
      prepared,
      confidence_level = confidence_level,
      nboot = nboot
    )
  }

  levels <- build_nested_three_level_sections(
    core = core,
    bootstrap_results = bootstrap_results,
    confidence_level = confidence_level,
    inference = inference,
    missing_data = missing_data,
    warnings = prepared$warnings
  )

  hierarchy <- prepared$hierarchy
  level_labels <- c(
    level1 = sprintf("Within %s", hierarchy$lower_level),
    level2 = sprintf(
      "Between %s within %s",
      hierarchy$lower_level,
      hierarchy$upper_level
    ),
    level3 = sprintf("Between %s", hierarchy$upper_level)
  )
  settings <- list(
    data = data,
    cluster = cluster,
    hierarchy = hierarchy,
    level_labels = level_labels,
    confidence_level = confidence_level,
    method = method,
    bootstrap = bootstrap,
    nboot = nboot,
    inference = inference,
    requested_inference = inference,
    weighted_between_statistics = FALSE,
    between_weighting = "equal_clusters",
    between_inference = "none",
    centering_rows = "pairwise_complete",
    missing_data = missing_data,
    auto_type = FALSE,
    var_type = prepared$var_type
  )
  centered_data <- list(
    analysis_data = prepared$analysis_data,
    lower_id = prepared$lower_id,
    upper_id = prepared$upper_id,
    hierarchy = hierarchy
  )
  empty_icc <- data.frame(
    variable = character(0),
    ICC = numeric(0),
    check.names = FALSE
  )

  output <- methods::new(
    "wbCorrNested",
    within = levels$level1,
    between = levels$level3,
    ICC = empty_icc,
    centered_data = centered_data,
    settings = settings,
    levels = levels
  )
  attr(output, "call") <- supplied_call
  output
}


compute_nested_three_level_bootstrap <- function(prepared,
                                                 confidence_level,
                                                 nboot) {
  variables <- colnames(prepared$analysis_data)
  combinations <- utils::combn(seq_along(variables), 2L)
  results <- vector("list", ncol(combinations))

  for (pair_index in seq_len(ncol(combinations))) {
    index1 <- combinations[1L, pair_index]
    index2 <- combinations[2L, pair_index]
    pair_results <- bootstrap_nested_three_level_pair(
      x = prepared$analysis_data[[index1]],
      y = prepared$analysis_data[[index2]],
      lower_id = prepared$lower_id,
      upper_id = prepared$upper_id,
      nboot = nboot,
      confidence_level = confidence_level
    )
    pair_results$Parameter1 <- variables[[index1]]
    pair_results$Parameter2 <- variables[[index2]]
    results[[pair_index]] <- pair_results
  }

  output <- do.call(rbind, results)
  rownames(output) <- NULL
  output
}


build_nested_three_level_sections <- function(core,
                                              bootstrap_results,
                                              confidence_level,
                                              inference,
                                              missing_data,
                                              warnings) {
  output <- list()
  level_names <- c("level1", "level2", "level3")

  for (level in level_names) {
    correlations <- core$correlations[[level]]
    p_values <- correlations
    p_values[,] <- NA_real_

    level_table <- core$table[
      core$table$level == level,
      ,
      drop = FALSE
    ]
    rownames(level_table) <- NULL
    level_table$warning <- vapply(
      seq_len(nrow(level_table)),
      function(index) {
        warning_text <- "None"
        for (parameter in c(
          level_table$Parameter1[[index]],
          level_table$Parameter2[[index]]
        )) {
          parameter_warning <- warnings[[parameter]]
          if (!is.null(parameter_warning) &&
              !is.na(parameter_warning) &&
              parameter_warning != "None") {
            warning_text <- append_cor_warning(
              warning_text,
              parameter_warning
            )
          }
        }
        warning_text
      },
      character(1)
    )
    level_table$n_boot_attempted <- NA_integer_
    level_table$n_boot_valid <- NA_integer_
    level_table$CI_lower <- NA_real_
    level_table$CI_upper <- NA_real_
    level_table$inference_status <- "not_requested"
    level_table$inference_reason <- NA_character_

    if (inference == "cluster_bootstrap") {
      level_bootstrap <- bootstrap_results[
        bootstrap_results$level == level,
        ,
        drop = FALSE
      ]
      matched <- integer(nrow(level_table))
      for (index in seq_len(nrow(level_table))) {
        pair_match <- which(
          level_bootstrap$Parameter1 == level_table$Parameter1[[index]] &
            level_bootstrap$Parameter2 == level_table$Parameter2[[index]]
        )
        if (length(pair_match) != 1L) {
          stop(
            "Internal error while matching three-level bootstrap results.",
            call. = FALSE
          )
        }
        matched[[index]] <- pair_match
      }
      level_table$n_boot_attempted <-
        level_bootstrap$n_boot_attempted[matched]
      level_table$n_boot_valid <- level_bootstrap$n_boot_valid[matched]
      level_table$CI_lower <- level_bootstrap$CI_lower[matched]
      level_table$CI_upper <- level_bootstrap$CI_upper[matched]
      level_table$inference_status <-
        level_bootstrap$inference_status[matched]
      level_table$inference_reason <-
        level_bootstrap$inference_reason[matched]

      not_estimable <- level_table$status != "ok"
      level_table$n_boot_attempted[not_estimable] <- 0L
      level_table$n_boot_valid[not_estimable] <- 0L
      level_table$CI_lower[not_estimable] <- NA_real_
      level_table$CI_upper[not_estimable] <- NA_real_
      level_table$inference_status[not_estimable] <- "unavailable"
      level_table$inference_reason[not_estimable] <-
        "coefficient_not_estimable"
    }

    confidence_intervals <- data.frame(
      Parameter1 = level_table$Parameter1,
      Parameter2 = level_table$Parameter2,
      CI_lower = level_table$CI_lower,
      correlation_coefficient = level_table$coefficient,
      CI_upper = level_table$CI_upper,
      stringsAsFactors = FALSE
    )

    display_table <- format_nested_three_level_table(
      level_table,
      confidence_level = confidence_level,
      inference = inference
    )
    matrix_diagnostics <- correlation_matrix_diagnostics(
      correlations,
      level = level,
      missing_data = missing_data,
      guaranteed_by_construction = missing_data == "listwise"
    )
    warn_non_psd_matrix(matrix_diagnostics)

    output[[level]] <- list(
      correlations = correlations,
      p_values = p_values,
      confidence_intervals = confidence_intervals,
      table = display_table,
      matrix_diagnostics = matrix_diagnostics
    )
  }

  output
}


format_nested_three_level_table <- function(table,
                                            confidence_level,
                                            inference) {
  table$level <- NULL
  table$method <- NULL
  table$n_lower_units <- NULL
  table$n_upper_units <- NULL
  names(table)[names(table) == "coefficient"] <- "pearson's r"
  table[["pearson's r"]] <- round(table[["pearson's r"]], 2)

  if (inference == "cluster_bootstrap") {
    ci_name <- paste0(
      "Cluster bootstrap ",
      confidence_level * 100,
      "% CI"
    )
    table[[ci_name]] <- ifelse(
      is.finite(table$CI_lower) & is.finite(table$CI_upper),
      sprintf("[%0.2f, %0.2f]", table$CI_lower, table$CI_upper),
      NA_character_
    )
  }
  table$CI_lower <- NULL
  table$CI_upper <- NULL
  if (all(table$warning == "None")) {
    table$warning <- NULL
  }

  preferred_order <- c(
    "Parameter1",
    "Parameter2",
    if ("warning" %in% colnames(table)) "warning",
    "pearson's r",
    if (inference == "cluster_bootstrap") {
      paste0("Cluster bootstrap ", confidence_level * 100, "% CI")
    },
    "n_obs",
    "n_units",
    "n_level2",
    "n_level3",
    "n_informative_units",
    "n_boot_attempted",
    "n_boot_valid",
    "status",
    "reason",
    "inference_status",
    "inference_reason"
  )
  table[, preferred_order, drop = FALSE]
}
