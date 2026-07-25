match_nested_plot_level <- function(level) {
  if (!is.character(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !level %in% c("level1", "l1", "level2", "l2", "level3", "l3")) {
    stop(
      paste0(
        "Invalid nested plot level. Select 'level1' or 'l1', ",
        "'level2' or 'l2', or 'level3' or 'l3'."
      ),
      call. = FALSE
    )
  }

  aliases <- c(
    level1 = "level1", l1 = "level1",
    level2 = "level2", l2 = "level2",
    level3 = "level3", l3 = "level3"
  )
  unname(aliases[[level]])
}


prepare_nested_plot_data <- function(wbCorrObject,
                                     level = c("level1",
                                               "level2",
                                               "level3"),
                                     standardize = TRUE,
                                     plot_NA = TRUE) {
  if (missing(level)) {
    level <- "level1"
  }
  level <- match_nested_plot_level(level)
  level_object <- wbCorrObject@levels[[level]]
  analysis_data <- wbCorrObject@centered_data$analysis_data
  lower_id <- wbCorrObject@centered_data$lower_id
  upper_id <- wbCorrObject@centered_data$upper_id

  variables <- colnames(level_object$correlations)
  if (!isTRUE(plot_NA)) {
    diagonal <- diag(as.matrix(level_object$correlations))
    variables <- names(diagonal)[is.finite(diagonal)]
  }
  if (length(variables) < 2L) {
    stop(
      sprintf("Fewer than two variables have plottable %s data.", level),
      call. = FALSE
    )
  }

  plot_pairs <- list()
  values_by_variable <- stats::setNames(
    vector("list", length(variables)),
    variables
  )
  combinations <- utils::combn(variables, 2L, simplify = FALSE)
  for (combination in combinations) {
    name_i <- combination[[1L]]
    name_j <- combination[[2L]]
    decomposition <- decompose_nested_three_level_pair(
      analysis_data[[name_i]],
      analysis_data[[name_j]],
      lower_id,
      upper_id
    )
    values <- decomposition[[level]]
    pair <- list(
      col_i = values$x,
      col_j = values$y,
      weights = NULL
    )
    if (isTRUE(standardize)) {
      pair <- standardize_wb_plot_pair(pair)
    }

    diagnostic <- decomposition$diagnostics[
      decomposition$diagnostics$level == level,
      ,
      drop = FALSE
    ]
    forward <- list(
      x = pair$col_i,
      y = pair$col_j,
      weights = NULL,
      n_obs = diagnostic$n_obs[[1L]],
      n_units = diagnostic$n_units[[1L]],
      n_level2 = diagnostic$n_level2[[1L]],
      n_level3 = diagnostic$n_level3[[1L]],
      method = "pearson"
    )
    reverse <- forward
    reverse$x <- pair$col_j
    reverse$y <- pair$col_i
    plot_pairs[[plot_pair_key(name_i, name_j)]] <- forward
    plot_pairs[[plot_pair_key(name_j, name_i)]] <- reverse

    values_by_variable[[name_i]] <- c(
      values_by_variable[[name_i]],
      pair$col_i
    )
    values_by_variable[[name_j]] <- c(
      values_by_variable[[name_j]],
      pair$col_j
    )
  }

  bounds <- lapply(values_by_variable, function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      return(c(-1, 1))
    }
    value_range <- range(values)
    if (value_range[[1L]] == value_range[[2L]]) {
      expansion <- max(abs(value_range[[1L]]), 1) * 0.05
      value_range <- value_range + c(-expansion, expansion)
    }
    value_range
  })
  frame <- as.data.frame(bounds, check.names = FALSE)
  frame <- encode_type(frame, wbCorrObject@settings$var_type)
  indices <- seq_len(ncol(frame)) / 100
  frame <- rbind(indices, -indices, frame)

  list(
    frame = frame,
    pairs = plot_pairs,
    variables = variables,
    level = level,
    level_object = level_object,
    method = "pearson",
    is_weighted = FALSE
  )
}


wb_plot_nested <- function(x, y = NULL, which = NULL,
                           plot_NA = TRUE,
                           standardize = TRUE,
                           outlier_detection = "zscore",
                           outlier_threshold = "recommended",
                           type = "p",
                           pch = 20, dot_lwd = 2,
                           reg_lwd = 2,
                           ...) {
  if (is.null(which)) {
    which <- y
  }
  level <- match_nested_plot_level(which)
  prepared <- prepare_nested_plot_data(
    x,
    level = level,
    standardize = standardize,
    plot_NA = plot_NA
  )
  plot_frame <- prepared$frame
  level_label <- x@settings$level_labels[[level]]
  if (is.null(level_label) ||
      length(level_label) != 1L ||
      is.na(level_label)) {
    level_label <- level
  }

  message("This may take a while...")
  graphics::pairs(
    plot_frame,
    main = sprintf("Bivariate associations: %s.", level_label),
    lower.panel = function(x_values, y_values, ...) {
      custom_lower_panel(
        x_values, y_values,
        type = type,
        method = prepared$method,
        auto_type = FALSE,
        var_type = x@settings$var_type,
        outlier_detection = outlier_detection,
        outlier_threshold = outlier_threshold,
        pch = pch,
        dot_lwd = dot_lwd,
        reg_lwd = reg_lwd,
        df = plot_frame,
        standardize = standardize,
        plot_NA = plot_NA,
        plot_pairs = prepared$pairs,
        ...
      )
    },
    upper.panel = function(x_values, y_values, ...) {
      custom_upper_panel(
        x_values, y_values,
        method = prepared$method,
        auto_type = FALSE,
        var_type = x@settings$var_type,
        wbCorrObject = prepared$level_object,
        is_weighted = FALSE,
        df = plot_frame,
        standardize = standardize,
        plot_NA = plot_NA,
        plot_pairs = prepared$pairs,
        ...
      )
    },
    ...
  )
  invisible(x)
}
