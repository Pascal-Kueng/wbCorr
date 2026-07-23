plot_parity_fixture <- function() {
  data.frame(
    id = c(rep(1:3, each = 3), NA),
    x = c(1, 2, 100, 2, 4, 6, 3, NA, 9, 999),
    y = c(1, NA, 3, 2, 4, 8, NA, 8, 12, -999),
    z = c(2, 4, 1, 5, 3, 9, 4, 7, 10, 500)
  )
}


get_prepared_plot_pair <- function(prepared, x_name, y_name) {
  key <- getFromNamespace("plot_pair_key", "wbCorr")(x_name, y_name)
  prepared$pairs[[key]]
}


test_that("plot data use exact pairwise rows and centering policy", {
  dat <- plot_parity_fixture()
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")

  pairwise <- suppressWarnings(
    wbCorr(dat, "id", inference = "none",
           centering_rows = "pairwise_complete")
  )
  pairwise_within <- get_prepared_plot_pair(
    prepare_plot(pairwise, "within", standardize = FALSE), "x", "y"
  )
  pairwise_between <- get_prepared_plot_pair(
    prepare_plot(pairwise, "between", standardize = FALSE), "x", "y"
  )

  keep <- complete.cases(dat$x, dat$y, dat$id)
  pair_rows <- dat[keep, ]
  expected_x_within <- pair_rows$x -
    ave(pair_rows$x, pair_rows$id, FUN = mean)
  expected_y_within <- pair_rows$y -
    ave(pair_rows$y, pair_rows$id, FUN = mean)
  expected_between <- aggregate(cbind(x, y) ~ id, pair_rows, mean)

  expect_equal(pairwise_within$x, expected_x_within)
  expect_equal(pairwise_within$y, expected_y_within)
  expect_identical(pairwise_within$n_obs, sum(keep))
  expect_identical(pairwise_within$n_clusters,
                   length(unique(pair_rows$id)))
  expect_equal(pairwise_between$x, expected_between$x)
  expect_equal(pairwise_between$y, expected_between$y)
  expect_null(pairwise_between$weights)
  expect_equal(cor(pairwise_within$x, pairwise_within$y),
               pairwise@within$correlations["x", "y"])
  expect_equal(cor(pairwise_between$x, pairwise_between$y),
               pairwise@between$correlations["x", "y"])
  expect_false(any(abs(pairwise_within$x) > 900))

  all_available <- suppressWarnings(
    wbCorr(dat, "id", inference = "none",
           centering_rows = "all_available")
  )
  available_within <- get_prepared_plot_pair(
    prepare_plot(all_available, "within", standardize = FALSE), "x", "y"
  )
  available_between <- get_prepared_plot_pair(
    prepare_plot(all_available, "between", standardize = FALSE), "x", "y"
  )
  x_means <- tapply(dat$x[!is.na(dat$id)],
                    dat$id[!is.na(dat$id)], mean, na.rm = TRUE)
  y_means <- tapply(dat$y[!is.na(dat$id)],
                    dat$id[!is.na(dat$id)], mean, na.rm = TRUE)
  expected_x_available <- pair_rows$x -
    as.numeric(x_means[as.character(pair_rows$id)])
  expected_y_available <- pair_rows$y -
    as.numeric(y_means[as.character(pair_rows$id)])

  expect_equal(available_within$x, expected_x_available)
  expect_equal(available_within$y, expected_y_available)
  expect_equal(available_between$x,
               as.numeric(x_means[as.character(expected_between$id)]))
  expect_equal(available_between$y,
               as.numeric(y_means[as.character(expected_between$id)]))
  expect_equal(cor(available_within$x, available_within$y),
               all_available@within$correlations["x", "y"])
  expect_equal(cor(available_between$x, available_between$y),
               all_available@between$correlations["x", "y"])
})


test_that("weighted between plot data and standardization preserve the estimand", {
  id <- rep(1:4, times = c(2, 3, 4, 5))
  dat <- data.frame(
    id = id,
    x = c(1, 3, 2, 3, 7, 4, 6, 5, 9, 8, 10, 11, 12, 15),
    y = c(5, 1, 3, 6, 4, 8, 7, 9, 5, 12, 10, 14, 11, 13)
  )
  result <- suppressWarnings(
    wbCorr(dat, "id", inference = "none",
           between_weighting = "cluster_size")
  )
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  weighted_cor_internal <- getFromNamespace("weighted_cor", "wbCorr")

  raw <- get_prepared_plot_pair(
    prepare_plot(result, "between", standardize = FALSE), "x", "y"
  )
  standardized <- get_prepared_plot_pair(
    prepare_plot(result, "between", standardize = TRUE), "x", "y"
  )
  standardized_prepared <- prepare_plot(result, "between", standardize = TRUE)
  lower_panel <- getFromNamespace("custom_lower_panel", "wbCorr")

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  panel <- lower_panel(
    standardized_prepared$frame$x,
    standardized_prepared$frame$y,
    method = "pearson",
    auto_type = FALSE,
    var_type = result@settings$var_type,
    outlier_detection = FALSE,
    outlier_threshold = "recommended",
    pch = 20,
    dot_lwd = 1,
    reg_lwd = 1,
    df = standardized_prepared$frame,
    standardize = TRUE,
    plot_NA = TRUE,
    plot_pairs = standardized_prepared$pairs
  )

  expect_identical(raw$weights, as.integer(table(id)))
  expect_equal(weighted_cor_internal(raw$x, raw$y, raw$weights),
               result@between$correlations["x", "y"])
  weighted_fit <- lm(standardized$y ~ standardized$x,
                     weights = standardized$weights)
  expect_equal(unname(coef(weighted_fit)[2]),
               result@between$correlations["x", "y"],
               tolerance = 1e-12)
  expect_identical(panel$weights, standardized$weights)
  expect_equal(unname(coef(panel$fit)[2]),
               result@between$correlations["x", "y"],
               tolerance = 1e-12)
})


test_that("plot panels use stored coefficients and exact prepared pairs", {
  dat <- plot_parity_fixture()
  result <- suppressWarnings(wbCorr(dat, "id", inference = "none"))
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  upper_panel <- getFromNamespace("custom_upper_panel", "wbCorr")
  lower_panel <- getFromNamespace("custom_lower_panel", "wbCorr")
  prepared <- prepare_plot(result, "within", standardize = TRUE)
  expected_slope <- result@within$correlations["x", "y"]
  result@within$correlations["x", "y"] <- 0.4321
  result@within$correlations["y", "x"] <- 0.4321
  result@within$p_values["x", "y"] <- 0.004
  result@within$p_values["y", "x"] <- 0.004
  prepared$level_object <- result@within
  x_values <- prepared$frame$x
  y_values <- prepared$frame$y

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  label <- upper_panel(
    x_values, y_values,
    method = "pearson",
    auto_type = FALSE,
    var_type = result@settings$var_type,
    wbCorrObject = result@within,
    is_weighted = FALSE,
    df = prepared$frame,
    standardize = TRUE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )
  panel <- lower_panel(
    x_values, y_values,
    method = "pearson",
    auto_type = FALSE,
    var_type = result@settings$var_type,
    outlier_detection = FALSE,
    outlier_threshold = "recommended",
    pch = 20,
    dot_lwd = 1,
    reg_lwd = 1,
    df = prepared$frame,
    standardize = TRUE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )

  expect_identical(
    label,
    "r = 0.43**"
  )
  expected_pair <- get_prepared_plot_pair(prepared, "x", "y")
  expect_equal(panel$x, expected_pair$x)
  expect_equal(panel$y, expected_pair$y)
  expect_null(panel$weights)
  expect_equal(unname(coef(panel$fit)[2]),
               expected_slope,
               tolerance = 1e-12)
})


test_that("Spearman plot pairs reproduce stored rank coefficients", {
  dat <- plot_parity_fixture()
  result <- suppressWarnings(
    wbCorr(dat, "id", method = "spearman", inference = "none")
  )
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  prepared <- prepare_plot(result, "within", standardize = FALSE)
  pair <- get_prepared_plot_pair(prepared, "x", "y")

  lower_panel <- getFromNamespace("custom_lower_panel", "wbCorr")
  upper_panel <- getFromNamespace("custom_upper_panel", "wbCorr")
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  panel <- lower_panel(
    prepared$frame$x, prepared$frame$y,
    method = "spearman",
    auto_type = FALSE,
    var_type = result@settings$var_type,
    outlier_detection = FALSE,
    outlier_threshold = "recommended",
    pch = 20,
    dot_lwd = 1,
    reg_lwd = 1,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )
  label <- upper_panel(
    prepared$frame$x, prepared$frame$y,
    method = "spearman",
    auto_type = FALSE,
    var_type = result@settings$var_type,
    wbCorrObject = result@within,
    is_weighted = FALSE,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )

  expect_equal(cor(pair$x, pair$y, method = "spearman"),
               result@within$correlations["x", "y"])
  expect_identical(pair$method, "spearman")
  expect_null(panel$fit)
  expect_identical(
    label,
    paste0("rho = ",
           sprintf("%.2f", result@within$correlations["x", "y"]))
  )
})


test_that("auto plotting follows each pair's realized stored method", {
  dat <- plot_parity_fixture()
  result <- suppressWarnings(
    wbCorr(dat, "id", method = "auto", inference = "none")
  )
  result@settings$var_type$x <- "ordinal"
  result@within$table$method[1] <- "centered-score Spearman rho"

  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  lower_panel <- getFromNamespace("custom_lower_panel", "wbCorr")
  upper_panel <- getFromNamespace("custom_upper_panel", "wbCorr")
  prepared <- prepare_plot(result, "within", standardize = FALSE)
  pair <- get_prepared_plot_pair(prepared, "x", "y")
  pearson_pair <- get_prepared_plot_pair(prepared, "x", "z")
  stored_rho <- cor(pair$x, pair$y, method = "spearman")
  result@within$correlations["x", "y"] <- stored_rho
  result@within$correlations["y", "x"] <- stored_rho
  prepared$level_object <- result@within

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  panel <- lower_panel(
    prepared$frame$x, prepared$frame$y,
    method = "auto",
    auto_type = TRUE,
    var_type = result@settings$var_type,
    outlier_detection = FALSE,
    outlier_threshold = "recommended",
    pch = 20,
    dot_lwd = 1,
    reg_lwd = 1,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )
  label <- upper_panel(
    prepared$frame$x, prepared$frame$y,
    method = "auto",
    auto_type = TRUE,
    var_type = result@settings$var_type,
    wbCorrObject = prepared$level_object,
    is_weighted = FALSE,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )
  pearson_panel <- lower_panel(
    prepared$frame$x, prepared$frame$z,
    method = "auto",
    auto_type = TRUE,
    var_type = result@settings$var_type,
    outlier_detection = FALSE,
    outlier_threshold = "recommended",
    pch = 20,
    dot_lwd = 1,
    reg_lwd = 1,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )
  pearson_label <- upper_panel(
    prepared$frame$x, prepared$frame$z,
    method = "auto",
    auto_type = TRUE,
    var_type = result@settings$var_type,
    wbCorrObject = prepared$level_object,
    is_weighted = FALSE,
    df = prepared$frame,
    standardize = FALSE,
    plot_NA = TRUE,
    plot_pairs = prepared$pairs
  )

  expect_identical(pair$method, "spearman")
  expect_null(panel$fit)
  expect_identical(label, paste0("rho = ", sprintf("%.2f", stored_rho)))
  expect_identical(pearson_pair$method, "pearson")
  expect_s3_class(pearson_panel$fit, "lm")
  expect_identical(
    pearson_label,
    paste0("r = ",
           sprintf("%.2f", result@within$correlations["x", "z"]))
  )
})


test_that("legacy plot-input fallback is exact or fails clearly", {
  dat <- plot_parity_fixture()
  named <- suppressWarnings(wbCorr(dat, "id", inference = "none"))
  named@centered_data$analysis_data <- NULL
  named@centered_data$cluster_var <- NULL
  named@settings$cluster <- NULL
  named@settings$data <- data.frame(
    unrelated_before = rev(seq_len(nrow(dat))),
    id = dat$id,
    unrelated_after = seq_len(nrow(dat)),
    dat[c("x", "y", "z")]
  )
  recover <- getFromNamespace("recover_wb_plot_inputs", "wbCorr")
  recovered <- recover(named)

  expect_equal(recovered$analysis_data$x,
               dat$x)
  expect_equal(as.character(recovered$cluster_var),
               as.character(dat$id))

  ambiguous <- named
  attr(ambiguous, "call") <- NULL
  expect_error(recover(ambiguous), "cannot be recovered unambiguously")

  external <- suppressWarnings(
    wbCorr(dat[c("x", "y", "z")], dat$id, inference = "none")
  )
  current_external <- recover(external)
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  external_pair <- get_prepared_plot_pair(
    prepare_plot(external, "within", standardize = FALSE), "x", "y"
  )
  expect_equal(current_external$analysis_data,
               external@centered_data$analysis_data)
  expect_equal(as.character(current_external$cluster_var),
               as.character(dat$id))
  expect_equal(cor(external_pair$x, external_pair$y),
               external@within$correlations["x", "y"])

  external@centered_data$analysis_data <- NULL
  external@centered_data$cluster_var <- NULL
  expect_error(recover(external), "cluster vector was not retained")
})


test_that("listwise plot pairs use the globally retained analysis rows", {
  dat <- plot_parity_fixture()
  dat$z[1] <- NA_real_
  result <- suppressWarnings(
    wbCorr(dat, "id", inference = "none", missing_data = "listwise")
  )
  prepare_plot <- getFromNamespace("prepare_wb_plot_data", "wbCorr")
  prepared <- prepare_plot(result, "within", standardize = FALSE)
  pair <- get_prepared_plot_pair(prepared, "x", "y")

  globally_complete <- complete.cases(dat[c("id", "x", "y", "z")])
  retained <- dat[globally_complete, ]
  expected_x <- retained$x - ave(retained$x, retained$id, FUN = mean)
  expected_y <- retained$y - ave(retained$y, retained$id, FUN = mean)

  expect_equal(result@centered_data$analysis_data,
               retained[c("x", "y", "z")],
               ignore_attr = TRUE)
  expect_equal(pair$x, expected_x)
  expect_equal(pair$y, expected_y)
  expect_identical(pair$n_obs, nrow(retained))
  expect_identical(pair$n_clusters, length(unique(retained$id)))
  expect_equal(cor(pair$x, pair$y),
               result@within$correlations["x", "y"])
})


test_that("public plotting succeeds for exact within and weighted-between data", {
  dat <- plot_parity_fixture()
  within <- suppressWarnings(wbCorr(dat, "id", inference = "none"))
  weighted <- suppressWarnings(
    wbCorr(dat, "id", inference = "none",
           between_weighting = "cluster_size")
  )

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_identical(
    suppressMessages(plot(within, "within", outlier_detection = FALSE)),
    within
  )
  expect_identical(
    suppressMessages(plot(weighted, "between", outlier_detection = FALSE)),
    weighted
  )
})
