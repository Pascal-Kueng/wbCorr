nested_plot_fixture <- function() {
  upper <- rep(seq_len(5L), each = 12L)
  lower_within_upper <- rep(rep(seq_len(3L), each = 4L), times = 5L)
  occasion <- rep(seq_len(4L), times = 15L)
  row <- seq_along(upper)

  data.frame(
    lower = interaction(upper, lower_within_upper, drop = TRUE),
    upper = upper,
    x = 0.8 * upper + 0.5 * lower_within_upper + occasion +
      sin(row),
    y = -0.4 * upper + 0.3 * lower_within_upper + 0.7 * occasion +
      cos(row * 0.7)
  )
}


nested_plot_object <- function() {
  wbCorr(
    nested_plot_fixture(),
    cluster = list(lower = "lower", upper = "upper"),
    inference = "none"
  )
}


get_nested_prepared_plot_pair <- function(prepared, x_name, y_name) {
  key <- getFromNamespace("plot_pair_key", "wbCorr")(x_name, y_name)
  prepared$pairs[[key]]
}


test_that("nested plot preparation reproduces every stored level estimand", {
  object <- nested_plot_object()
  prepare_plot <- getFromNamespace("prepare_nested_plot_data", "wbCorr")
  decompose_pair <- getFromNamespace(
    "decompose_nested_three_level_pair",
    "wbCorr"
  )
  decomposition <- decompose_pair(
    object@centered_data$analysis_data$x,
    object@centered_data$analysis_data$y,
    object@centered_data$lower_id,
    object@centered_data$upper_id
  )

  for (level in c("level1", "level2", "level3")) {
    prepared <- prepare_plot(
      object,
      level = level,
      standardize = FALSE
    )
    pair <- get_nested_prepared_plot_pair(prepared, "x", "y")
    reverse_pair <- get_nested_prepared_plot_pair(prepared, "y", "x")
    diagnostic <- decomposition$diagnostics[
      decomposition$diagnostics$level == level,
      ,
      drop = FALSE
    ]

    expect_equal(pair$x, decomposition[[level]]$x)
    expect_equal(pair$y, decomposition[[level]]$y)
    expect_null(pair$weights)
    expect_identical(pair$method, "pearson")
    expect_identical(pair$n_obs, diagnostic$n_obs[[1L]])
    expect_identical(pair$n_units, diagnostic$n_units[[1L]])
    expect_equal(reverse_pair$x, pair$y)
    expect_equal(reverse_pair$y, pair$x)
    expect_equal(
      cor(pair$x, pair$y),
      object@levels[[level]]$correlations["x", "y"],
      tolerance = 1e-12
    )
  }
})


test_that("nested plot level aliases and standardization preserve correlations", {
  object <- nested_plot_object()
  prepare_plot <- getFromNamespace("prepare_nested_plot_data", "wbCorr")

  for (selector in c("l1", "l2", "l3")) {
    prepared <- prepare_plot(object, level = selector, standardize = TRUE)
    pair <- get_nested_prepared_plot_pair(prepared, "x", "y")
    fit <- lm(pair$y ~ pair$x)
    level <- paste0("level", substring(selector, 2L))

    expect_identical(prepared$level, level)
    expect_equal(
      unname(coef(fit)[[2L]]),
      object@levels[[level]]$correlations["x", "y"],
      tolerance = 1e-12
    )
    expect_equal(mean(pair$x), 0, tolerance = 1e-12)
    expect_equal(mean(pair$y), 0, tolerance = 1e-12)
  }

  expect_error(
    prepare_plot(object, level = "within"),
    "Invalid nested plot level"
  )
})


test_that("nested plotting accepts all level selectors and returns its object", {
  object <- nested_plot_object()
  nested_plot <- getFromNamespace("wb_plot_nested", "wbCorr")

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  for (selector in c("level1", "l1", "level2", "l2", "level3", "l3")) {
    expect_identical(
      suppressMessages(
        nested_plot(object, selector, outlier_detection = FALSE)
      ),
      object
    )
  }
  expect_identical(
    suppressMessages(plot(object, "l1", outlier_detection = FALSE)),
    object
  )
  expect_identical(
    suppressMessages(
      plot(object, which = "l2", outlier_detection = FALSE)
    ),
    object
  )

  expect_error(nested_plot(object), "Invalid nested plot level")
  expect_error(
    nested_plot(object, "l1", which = "between"),
    "Invalid nested plot level"
  )
})


test_that("nested plot_NA excludes variables degenerate at the chosen level", {
  data <- nested_plot_fixture()
  data$upper_only <- data$upper
  object <- wbCorr(
    data,
    cluster = list(lower = "lower", upper = "upper"),
    inference = "none"
  )
  prepare_plot <- getFromNamespace("prepare_nested_plot_data", "wbCorr")

  prepared <- prepare_plot(
    object,
    level = "level1",
    plot_NA = FALSE
  )
  expect_setequal(prepared$variables, c("x", "y"))
  expect_false("upper_only" %in% colnames(prepared$frame))
})
