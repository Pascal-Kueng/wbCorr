test_that("Spearman coefficients match the documented descriptive estimands", {
  dat <- data.frame(
    id = rep(1:6, each = 4),
    x = c(1, 4, 2, 7, 2, 6, 3, 8, 4, 9, 5, 7,
          3, 8, 6, 10, 5, 11, 7, 9, 6, 12, 8, 13),
    y = c(8, 3, 7, 2, 7, 5, 9, 1, 6, 4, 8, 3,
          9, 2, 7, 5, 8, 1, 6, 4, 10, 3, 9, 2)
  )

  result <- wbCorr(dat, "id", method = "spearman", inference = "none")
  x_centered <- dat$x - ave(dat$x, dat$id, FUN = mean)
  y_centered <- dat$y - ave(dat$y, dat$id, FUN = mean)
  means <- aggregate(cbind(x, y) ~ id, dat, mean)

  expect_equal(result@within$correlations["x", "y"],
               cor(x_centered, y_centered, method = "spearman"))
  expect_equal(result@between$correlations["x", "y"],
               cor(means$x, means$y, method = "spearman"))
  expect_true("centered-score Spearman rho" %in%
                colnames(result@within$table))
  expect_true("cluster-mean Spearman rho" %in%
                colnames(result@between$table))
})


test_that("analytic Spearman requests retain coefficients but omit inference", {
  dat <- data.frame(
    id = rep(1:3, each = 2),
    x = c(1, 4, 2, 6, 3, 5),
    y = c(2, 5, 4, 1, 6, 3)
  )

  expect_warning(
    result <- wbCorr(dat, "id", method = "spearman"),
    "Analytic inference is not supported"
  )
  point_only <- wbCorr(dat, "id", method = "spearman", inference = "none")

  expect_identical(result@settings$inference, "none")
  expect_identical(result@settings$requested_inference, "analytic")
  expect_identical(point_only@settings$requested_inference, "none")
  expect_true(all(result@within$table$inference_status == "unavailable"))
  expect_true(all(result@within$table$inference_reason ==
                    "analytic_inference_unsupported_for_method"))
  expect_true(all(point_only@within$table$inference_status ==
                    "not_requested"))
  expect_equal(result@within$correlations, point_only@within$correlations)
  expect_equal(result@between$correlations, point_only@between$correlations)
  expect_true(all(is.na(result@within$p_values)))
  expect_true(all(is.na(result@between$p_values)))
  expect_equal(result@between$correlations["x", "y"],
               cor(c(2.5, 4, 4), c(3.5, 2.5, 4.5), method = "spearman"))
  expect_false(any(c("df", "p", "z-statistic", "95% CI") %in%
                     colnames(result@within$table)))

  matrices <- get_matrix(result)
  expect_s3_class(matrices$within, "data.frame")
  expect_false("note" %in% names(matrices))

  spearman_from_r <- getFromNamespace("cor_spearman_from_r", "wbCorr")
  unavailable <- spearman_from_r(0.5, 0, 0.95)
  expect_equal(unavailable$correlation_coefficient, 0.5)
  expect_true(all(is.na(unlist(unavailable[-1]))))
})


test_that("row jackknife and weighted Spearman fail with migration guidance", {
  dat <- data.frame(
    id = rep(1:4, each = 3),
    x = 1:12,
    y = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11)
  )

  expect_error(
    wbCorr(dat, "id", method = "spearman-jackknife"),
    "not supported.*cluster_bootstrap"
  )
  expect_error(
    wbCorr(dat, "id", method = "spearman",
           between_weighting = "cluster_size", inference = "none"),
    "cluster_size.*not supported.*spearman"
  )
})


test_that("cluster bootstrap supplies Spearman intervals but no pseudo p-value", {
  dat <- data.frame(
    id = rep(1:8, each = 4),
    x = rep(1:8, each = 4) + rep(c(-2, -0.5, 0.5, 2), 8),
    y = rep(c(1, 3, 2, 5, 4, 7, 6, 9), each = 4) +
      rep(c(-1, 0, 0.5, 1.5), 8)
  )

  set.seed(42)
  result <- wbCorr(dat,
                   "id",
                   method = "spearman",
                   inference = "cluster_bootstrap",
                   nboot = 30)

  expect_false(is.na(result@within$confidence_intervals$CI_lower[1]))
  expect_false(is.na(result@between$confidence_intervals$CI_lower[1]))
  expect_true(all(is.na(result@within$p_values)))
  expect_true(all(is.na(result@between$p_values)))
  expect_false(any(grepl("bootstrap p", colnames(result@within$table),
                         ignore.case = TRUE)))

  matrices <- get_matrix(result)
  expect_s3_class(matrices$between, "data.frame")
  expect_false("note" %in% names(matrices))
})


test_that("Spearman bootstrap matches seeded whole-cluster resampling", {
  cluster_sizes <- 2:8
  id <- rep(seq_along(cluster_sizes), cluster_sizes)
  occasion <- sequence(cluster_sizes)
  cluster_y <- c(5, 1, 7, 3, 8, 2, 6)
  dat <- data.frame(
    id = id,
    x = 2 * id + occasion + ((id + occasion) %% 3) / 5,
    y = cluster_y[id] + (-1)^id * occasion + ((2 * occasion + id) %% 4) / 4
  )
  dat$x[c(5, 19)] <- NA_real_
  dat$y[c(10, 27)] <- NA_real_

  nboot <- 40L
  confidence_level <- 0.90
  set.seed(9182)
  result <- wbCorr(dat,
                   "id",
                   method = "spearman",
                   inference = "cluster_bootstrap",
                   confidence_level = confidence_level,
                   nboot = nboot)

  clusters <- levels(factor(dat$id))
  complete <- complete.cases(dat$x, dat$y, dat$id)
  draw_correlation <- function(sampled_clusters, level) {
    sampled_x <- numeric()
    sampled_y <- numeric()
    for (sampled_cluster in sampled_clusters) {
      rows <- which(as.character(dat$id) == sampled_cluster & complete)
      x <- dat$x[rows]
      y <- dat$y[rows]
      if (level == "within") {
        sampled_x <- c(sampled_x, x - mean(x))
        sampled_y <- c(sampled_y, y - mean(y))
      } else {
        sampled_x <- c(sampled_x, mean(x))
        sampled_y <- c(sampled_y, mean(y))
      }
    }
    suppressWarnings(cor(sampled_x, sampled_y, method = "spearman"))
  }

  set.seed(9182)
  within_draws <- replicate(
    nboot,
    draw_correlation(sample(clusters, length(clusters), replace = TRUE),
                     "within")
  )
  between_draws <- replicate(
    nboot,
    draw_correlation(sample(clusters, length(clusters), replace = TRUE),
                     "between")
  )
  interval_probs <- c((1 - confidence_level) / 2,
                      1 - (1 - confidence_level) / 2)

  expect_equal(
    unname(unlist(result@within$confidence_intervals[1, c("CI_lower",
                                                          "CI_upper")])),
    unname(quantile(within_draws, interval_probs, na.rm = TRUE))
  )
  expect_equal(
    unname(unlist(result@between$confidence_intervals[1, c("CI_lower",
                                                           "CI_upper")])),
    unname(quantile(between_draws, interval_probs, na.rm = TRUE))
  )
  expect_identical(result@within$table$n_boot_attempted, nboot)
  expect_identical(result@between$table$n_boot_attempted, nboot)
  expect_identical(result@within$table$n_boot_valid,
                   as.integer(sum(!is.na(within_draws))))
  expect_identical(result@between$table$n_boot_valid,
                   as.integer(sum(!is.na(between_draws))))

  expected_status <- function(draws) {
    n_valid <- sum(!is.na(draws))
    if (n_valid < 10L) {
      "unavailable"
    } else if (n_valid < nboot) {
      "partial"
    } else {
      "ok"
    }
  }
  expect_identical(result@within$table$inference_status,
                   expected_status(within_draws))
  expect_identical(result@between$table$inference_status,
                   expected_status(between_draws))
})


test_that("Spearman plot annotations contain rho and no analytic stars", {
  upper_panel <- getFromNamespace("custom_upper_panel", "wbCorr")
  p_values <- data.frame(x = c(NA_real_, NA_real_),
                         y = c(NA_real_, NA_real_),
                         row.names = c("x", "y"))
  correlations <- data.frame(x = c(1, 0.125),
                             y = c(0.125, 1),
                             row.names = c("x", "y"))
  panel_data <- data.frame(x = numeric(), y = numeric())
  x <- c(0.01, -0.01, 0.01, -0.01, 1, 2, 3, 4)
  y <- c(0.02, -0.02, 0.01, -0.01, 4, 1, 3, 2)

  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::plot.new()
  label <- upper_panel(x,
                       y,
                       method = "spearman",
                       auto_type = FALSE,
                       var_type = NULL,
                       wbCorrObject = list(p_values = p_values,
                                           correlations = correlations),
                       is_weighted = FALSE,
                       df = panel_data,
                       standardize = TRUE,
                       plot_NA = TRUE)

  expect_identical(label, "rho = 0.12")
  expect_false(grepl("\\*", label))
})
