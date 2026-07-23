
recover_wb_plot_inputs <- function(wbCorrObject) {
  analysis_names <- colnames(wbCorrObject@within$correlations)
  analysis_data <- wbCorrObject@centered_data$analysis_data
  cluster_var <- wbCorrObject@centered_data$cluster_var

  if (is.data.frame(analysis_data) &&
      all(analysis_names %in% colnames(analysis_data)) &&
      length(cluster_var) == nrow(analysis_data)) {
    return(list(analysis_data = analysis_data[, analysis_names, drop = FALSE],
                cluster_var = cluster_var))
  }

  # Compatibility for objects created before normalized plotting inputs were
  # stored. This is exact when the original data still contain the cluster
  # column, which is the historical and documented common case.
  raw_data <- wbCorrObject@settings$data
  if (!is.data.frame(raw_data) ||
      !all(analysis_names %in% colnames(raw_data))) {
    stop("Exact plot data are unavailable in this wbCorr object. Refit it with the current wbCorr version.",
         call. = FALSE)
  }

  cluster_candidates <- setdiff(colnames(raw_data), analysis_names)
  if (length(cluster_candidates) < 1L) {
    stop("The cluster vector was not retained in this older wbCorr object. Refit it with the current wbCorr version before plotting.",
         call. = FALSE)
  }

  named_cluster <- wbCorrObject@settings$cluster
  if (!is.character(named_cluster) ||
      length(named_cluster) != 1L ||
      is.na(named_cluster) ||
      !named_cluster %in% cluster_candidates) {
    named_cluster <- NULL
  }
  stored_call <- attr(wbCorrObject, "call")
  if (is.null(named_cluster) && !is.null(stored_call)) {
    called_cluster <- stored_call$cluster
    if (is.character(called_cluster) &&
        length(called_cluster) == 1L &&
        !is.na(called_cluster) &&
        called_cluster %in% cluster_candidates) {
      named_cluster <- called_cluster
    }
  }
  if (is.null(named_cluster)) {
    if (length(cluster_candidates) != 1L) {
      stop("The cluster column cannot be recovered unambiguously from this older wbCorr object. Refit it with the current wbCorr version before plotting.",
           call. = FALSE)
    }
    named_cluster <- cluster_candidates[[1L]]
  }

  normalized_data <- raw_data[, analysis_names, drop = FALSE]
  for (name in analysis_names) {
    checked <- suppressWarnings(
      check_assumptions(normalized_data[[name]],
                        name,
                        wbCorrObject@settings$method)
    )
    normalized_data[[name]] <- checked$col
  }

  list(analysis_data = normalized_data,
       cluster_var = as.factor(raw_data[[named_cluster]]))
}


standardize_wb_plot_pair <- function(pair) {
  weights <- pair$weights

  standardize_one <- function(values) {
    if (length(values) < 2L) {
      return(values)
    }
    effective_weights <- if (is.null(weights)) {
      rep(1, length(values))
    } else {
      weights
    }
    keep <- is.finite(values) & is.finite(effective_weights) &
      effective_weights > 0
    if (sum(keep) < 2L) {
      return(values)
    }
    weighted_mean <- sum(effective_weights[keep] * values[keep]) /
      sum(effective_weights[keep])
    weighted_variance <- sum(effective_weights[keep] *
                               (values[keep] - weighted_mean)^2) /
      sum(effective_weights[keep])
    if (!is.finite(weighted_variance) || weighted_variance <= 0) {
      return(values)
    }
    (values - weighted_mean) / sqrt(weighted_variance)
  }

  pair$col_i <- standardize_one(pair$col_i)
  pair$col_j <- standardize_one(pair$col_j)
  pair
}


wb_plot_pair_method <- function(wbCorrObject,
                                level_object,
                                name_i,
                                name_j) {
  result_table <- level_object$table
  required_columns <- c("Parameter1", "Parameter2", "method")
  if (is.data.frame(result_table) &&
      all(required_columns %in% colnames(result_table))) {
    matching_row <- (result_table$Parameter1 == name_i &
                       result_table$Parameter2 == name_j) |
      (result_table$Parameter1 == name_j &
         result_table$Parameter2 == name_i)
    stored_method <- result_table$method[which(matching_row)[1L]]
    if (length(stored_method) == 1L && !is.na(stored_method)) {
      if (grepl("spearman", stored_method, ignore.case = TRUE)) {
        return("spearman")
      }
      if (grepl("pearson", stored_method, ignore.case = TRUE)) {
        return("pearson")
      }
    }
  }

  requested_method <- wbCorrObject@settings$method
  if (identical(requested_method, "spearman")) {
    return("spearman")
  }
  if (identical(requested_method, "auto")) {
    variable_types <- wbCorrObject@settings$var_type[c(name_i, name_j)]
    if (any(unlist(variable_types, use.names = FALSE) == "ordinal",
            na.rm = TRUE)) {
      return("spearman")
    }
  }
  "pearson"
}


prepare_wb_plot_data <- function(wbCorrObject,
                                 level = c("within", "between"),
                                 standardize = TRUE,
                                 plot_NA = TRUE) {
  level <- match.arg(level)
  inputs <- recover_wb_plot_inputs(wbCorrObject)
  analysis_data <- inputs$analysis_data
  cluster_var <- inputs$cluster_var
  level_object <- if (level == "within") {
    wbCorrObject@within
  } else {
    wbCorrObject@between
  }

  variables <- colnames(level_object$correlations)
  if (!isTRUE(plot_NA)) {
    diagonal <- diag(as.matrix(level_object$correlations))
    variables <- names(diagonal)[is.finite(diagonal)]
  }
  if (length(variables) < 2L) {
    stop(sprintf("Fewer than two variables have plottable %s-cluster data.",
                 level),
         call. = FALSE)
  }

  plot_pairs <- list()
  values_by_variable <- setNames(vector("list", length(variables)), variables)
  combinations <- utils::combn(variables, 2, simplify = FALSE)
  for (combination in combinations) {
    name_i <- combination[[1L]]
    name_j <- combination[[2L]]
    pair <- prepare_correlation_pair(
      analysis_data[[name_i]],
      analysis_data[[name_j]],
      cluster_var,
      level,
      wbCorrObject@settings$between_weighting,
      wbCorrObject@settings$centering_rows
    )
    pair_method <- wb_plot_pair_method(wbCorrObject,
                                       level_object,
                                       name_i,
                                       name_j)
    if (isTRUE(standardize)) {
      pair <- standardize_wb_plot_pair(pair)
    }

    forward <- list(x = pair$col_i,
                    y = pair$col_j,
                    weights = pair$weights,
                    n_obs = pair$n_obs,
                    n_clusters = pair$n_clusters,
                    method = pair_method)
    reverse <- list(x = pair$col_j,
                    y = pair$col_i,
                    weights = pair$weights,
                    n_obs = pair$n_obs,
                    n_clusters = pair$n_clusters,
                    method = pair_method)
    plot_pairs[[plot_pair_key(name_i, name_j)]] <- forward
    plot_pairs[[plot_pair_key(name_j, name_i)]] <- reverse

    values_by_variable[[name_i]] <- c(values_by_variable[[name_i]],
                                      pair$col_i)
    values_by_variable[[name_j]] <- c(values_by_variable[[name_j]],
                                      pair$col_j)
  }

  bounds <- lapply(values_by_variable, function(values) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      return(c(-1, 1))
    }
    value_range <- range(values)
    if (value_range[1] == value_range[2]) {
      expansion <- max(abs(value_range[1]), 1) * 0.05
      value_range <- value_range + c(-expansion, expansion)
    }
    value_range
  })
  frame <- as.data.frame(bounds, check.names = FALSE)
  frame <- encode_type(frame, wbCorrObject@settings$var_type)
  indices <- seq_len(ncol(frame)) / 100
  frame <- rbind(indices, -indices, frame)

  list(frame = frame,
       pairs = plot_pairs,
       variables = variables,
       level_object = level_object,
       method = wbCorrObject@settings$method,
       is_weighted = level == "between" &&
         wbCorrObject@settings$between_weighting == "cluster_size")
}


#' @importFrom graphics abline pairs par points text
#' @importFrom stats coef lm median sd setNames
wb_plot <- function(x, y, which = NULL,
                    plot_NA = TRUE,
                    standardize = TRUE,
                    outlier_detection = 'zscore',
                    outlier_threshold = 'recommended',
                    type = "p",
                    pch = 20, dot_lwd = 2,
                    reg_lwd = 2,
                    ...) {
  if (is.null(which)) {
    which <- y
  }
  if (length(which) != 1L ||
      !is.character(which) ||
      is.na(which) ||
      !which %in% c("w", "within", "b", "between")) {
    stop("Invalid value for the 'which' argument. Allowed values are 'w', 'within', 'b', and 'between'.",
         call. = FALSE)
  }

  level <- if (which %in% c("w", "within")) "within" else "between"
  prepared <- prepare_wb_plot_data(x,
                                   level = level,
                                   standardize = standardize,
                                   plot_NA = plot_NA)
  plot_frame <- prepared$frame
  method <- prepared$method
  var_type <- x@settings$var_type
  auto_type <- x@settings$auto_type

  message("This may take a while...")
  graphics::pairs(
    plot_frame,
    main = sprintf("Bivariate associations of %s-cluster %s.",
                   level,
                   if (level == "within") {
                     "centered scores"
                   } else {
                     "means"
                   }),
    lower.panel = function(x_values, y_values, ...) {
      custom_lower_panel(
        x_values, y_values,
        type = type,
        method = method,
        auto_type = auto_type,
        var_type = var_type,
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
        method = method,
        auto_type = auto_type,
        var_type = var_type,
        wbCorrObject = prepared$level_object,
        is_weighted = prepared$is_weighted,
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
