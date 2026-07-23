test_that("every requested pair is retained when coefficients are unavailable", {
  dat <- data.frame(
    id = 1:4,
    x = c(1, 2, NA, NA),
    y = c(NA, 3, 4, NA),
    z = c(NA, NA, 5, 6)
  )

  result <- wbCorr(dat, "id", inference = "none")
  expected_pairs <- data.frame(
    Parameter1 = c("x", "x", "y"),
    Parameter2 = c("y", "z", "z")
  )

  for (level in c("within", "between")) {
    table <- methods::slot(result, level)$table
    intervals <- methods::slot(result, level)$confidence_intervals
    expect_equal(table[, c("Parameter1", "Parameter2")], expected_pairs)
    expect_equal(nrow(table), choose(3, 2))
    expect_equal(nrow(intervals), choose(3, 2))
    expect_equal(table$n_obs, c(1L, 0L, 1L))
    expect_equal(table$n_clusters, c(1L, 0L, 1L))
    expect_true(all(table$status == "not_estimable"))
    expect_true("pearson's r" %in% colnames(table))
    expect_true(all(is.na(table[["pearson's r"]])))
    expect_true(all(table$inference_status == "not_requested"))
    expect_true(all(is.na(intervals$correlation_coefficient)))
  }

  expect_true(all(result@within$table$reason ==
                    "fewer_than_two_complete_observations"))
  expect_true(all(result@between$table$reason ==
                    "fewer_than_two_contributing_clusters"))
})


test_that("two analysis units retain coefficients but not unsupported inference", {
  one_cluster <- data.frame(
    id = c(1, 1),
    x = c(1, 3),
    y = c(4, 2)
  )
  result <- suppressWarnings(wbCorr(one_cluster, "id"))

  expect_equal(result@within$correlations["x", "y"], -1)
  expect_identical(result@within$table$n_obs, 2L)
  expect_identical(result@within$table$n_clusters, 1L)
  expect_identical(result@within$table$status, "ok")
  expect_identical(result@within$table$inference_status, "unavailable")
  expect_identical(result@within$table$inference_reason,
                   "analytic_inference_unavailable_low_df")
  expect_identical(result@between$table$status, "not_estimable")
  expect_identical(result@between$table$reason,
                   "fewer_than_two_contributing_clusters")

  two_clusters <- data.frame(
    id = rep(1:2, each = 2),
    x = c(0, 2, 10, 12),
    y = c(0, 4, 5, 9)
  )
  point_only <- wbCorr(two_clusters, "id", inference = "none")
  weighted <- wbCorr(two_clusters,
                     "id",
                     inference = "none",
                     between_weighting = "cluster_size")
  analytic <- suppressWarnings(wbCorr(two_clusters, "id"))

  expect_equal(point_only@between$correlations["x", "y"], 1)
  expect_equal(weighted@between$correlations["x", "y"], 1)
  expect_equal(analytic@between$correlations["x", "y"], 1)
  expect_identical(point_only@between$table$n_obs, 4L)
  expect_identical(point_only@between$table$n_clusters, 2L)
  expect_identical(point_only@between$table$status, "ok")
  expect_identical(point_only@between$table$inference_status,
                   "not_requested")
  expect_identical(analytic@between$table$inference_status, "unavailable")
})


test_that("pair diagnostics identify level-specific zero variance", {
  id <- rep(1:4, each = 3)
  dat <- data.frame(
    id = id,
    between_only = rep(1:4, each = 3),
    within_only = rep(c(-1, 0, 1), 4),
    both = rep(1:4, each = 3) + rep(c(-1, 0, 1), 4)
  )

  result <- wbCorr(dat, "id", inference = "none")
  within <- result@within$table
  between <- result@between$table

  within_pair <- within$Parameter1 == "between_only" &
    within$Parameter2 == "within_only"
  between_pair <- between$Parameter1 == "between_only" &
    between$Parameter2 == "within_only"
  expect_identical(within$status[within_pair], "not_estimable")
  expect_identical(within$reason[within_pair], "zero_variance_parameter1")
  expect_identical(between$status[between_pair], "not_estimable")
  expect_identical(between$reason[between_pair], "zero_variance_parameter2")
  expect_true(is.na(result@within$correlations["between_only", "within_only"]))
  expect_true(is.na(result@between$correlations["between_only", "within_only"]))
})


test_that("counts use complete raw rows and contributing clusters", {
  dat <- data.frame(
    id = c(1, 1, 2, 2, 3, NA),
    x = c(1, 2, 3, NA, 5, 9),
    y = c(2, NA, 4, 5, 6, 10)
  )

  pairwise <- suppressWarnings(wbCorr(dat, "id"))
  all_available <- suppressWarnings(
    wbCorr(dat, "id", centering_rows = "all_available")
  )

  for (result in list(pairwise, all_available)) {
    expect_identical(result@within$table$n_obs, 3L)
    expect_identical(result@within$table$n_clusters, 3L)
    expect_identical(result@between$table$n_obs, 3L)
    expect_identical(result@between$table$n_clusters, 3L)
  }
})


test_that("correlation diagonals reflect variance at each level", {
  id <- rep(1:4, each = 3)
  dat <- data.frame(
    id = id,
    between_only = rep(1:4, each = 3),
    within_only = rep(c(-1, 0, 1), 4),
    both = rep(1:4, each = 3) + rep(c(-1, 0, 1), 4),
    constant = rep(0, length(id)),
    binary_between = factor(
      rep(c("no", "no", "yes", "yes"), each = 3),
      levels = c("no", "yes")
    ),
    binary_within = factor(
      rep(c("no", "yes", "no"), 4),
      levels = c("no", "yes")
    )
  )

  result <- suppressWarnings(
    wbCorr(dat, "id", method = "auto", inference = "none")
  )
  expected_within <- c(NA, 1, 1, NA, NA, 1)
  expected_between <- c(1, NA, 1, NA, 1, NA)
  names(expected_within) <- names(expected_between) <- names(dat)[-1]

  expect_equal(diag(as.matrix(result@within$correlations)), expected_within)
  expect_equal(diag(as.matrix(result@between$correlations)), expected_between)
  expect_true(all(is.na(diag(as.matrix(result@within$p_values)))))
  expect_true(all(is.na(diag(as.matrix(result@between$p_values)))))

  matrices <- get_matrix(result)
  expect_identical(diag(as.matrix(matrices$within)),
                   ifelse(is.na(expected_within), "NA", "1.00"))
  expect_identical(diag(as.matrix(matrices$between)),
                   ifelse(is.na(expected_between), "NA", "1.00"))

  expected_icc <- sprintf("[%0.2f]", result@ICC$ICC)
  expect_identical(unname(diag(as.matrix(matrices$merged_wb))), expected_icc)
  expect_identical(unname(diag(as.matrix(matrices$merged_bw))), expected_icc)
})


test_that("diagonals use own observed rows and always omit p-values", {
  dat <- data.frame(
    id = c(NA, NA, rep(1:3, each = 2)),
    x = c(0, 1, rep(5, 6)),
    companion = c(NA, 1, 1, NA, 2, NA, 3, NA)
  )
  none <- wbCorr(dat, "id", inference = "none")
  analytic <- suppressWarnings(wbCorr(dat, "id"))
  set.seed(12)
  bootstrap <- wbCorr(dat,
                      "id",
                      inference = "cluster_bootstrap",
                      nboot = 10)

  expect_true(is.na(none@within$correlations["x", "x"]))
  expect_true(is.na(none@between$correlations["x", "x"]))
  for (result in list(none, analytic, bootstrap)) {
    expect_true(all(is.na(diag(as.matrix(result@within$p_values)))))
    expect_true(all(is.na(diag(as.matrix(result@between$p_values)))))
  }
})


test_that("diagnostic columns have a stable schema", {
  dat <- data.frame(
    id = rep(1:4, each = 3),
    x = 1:12,
    y = c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11)
  )
  result <- wbCorr(dat, "id", inference = "none")
  diagnostics <- c("n_obs", "n_clusters", "n_boot_attempted",
                   "n_boot_valid", "status", "reason", "inference_status",
                   "inference_reason")

  expect_true(all(diagnostics %in% colnames(result@within$table)))
  expect_true(all(diagnostics %in% colnames(result@between$table)))
  expect_identical(result@within$table$status, "ok")
  expect_identical(result@within$table$inference_status, "not_requested")
  expect_true(is.na(result@within$table$reason))
  expect_true(is.na(result@within$table$inference_reason))
})


test_that("bootstrap diagnostics distinguish attempted, valid, and skipped draws", {
  dat <- data.frame(
    id = rep(1:3, each = 3),
    x = c(0, 1, 2, 10, 11, 12, 20, 21, 22),
    y = c(0, 2, 1, 8, 10, 9, 25, 23, 24)
  )

  set.seed(1)
  pearson <- wbCorr(dat,
                    "id",
                    inference = "cluster_bootstrap",
                    nboot = 30)
  set.seed(1)
  spearman <- wbCorr(dat,
                     "id",
                     method = "spearman",
                     inference = "cluster_bootstrap",
                     nboot = 30)
  set.seed(1)
  between_flag_ignored <- wbCorr(dat,
                                 "id",
                                 inference = "cluster_bootstrap",
                                 between_inference = "none",
                                 nboot = 30)

  expect_identical(pearson@between$table$n_boot_attempted, 30L)
  expect_identical(pearson@between$table$n_boot_valid, 25L)
  expect_identical(pearson@between$table$inference_status, "partial")
  expect_identical(pearson@between$table$inference_reason,
                   "invalid_bootstrap_replicates_excluded")
  expect_match(pearson@between$table$warning,
               "25 of 30 cluster-bootstrap replicates")
  expect_identical(spearman@between$table$n_boot_valid,
                   pearson@between$table$n_boot_valid)
  expect_identical(spearman@between$table$inference_status,
                   pearson@between$table$inference_status)
  expect_identical(between_flag_ignored@between$table$n_boot_attempted, 30L)
  expect_equal(between_flag_ignored@between$confidence_intervals,
               pearson@between$confidence_intervals)
  expect_true(all(is.na(pearson@within$p_values)))
  expect_true(all(is.na(pearson@between$p_values)))

  set.seed(1)
  too_few_valid <- wbCorr(dat,
                          "id",
                          inference = "cluster_bootstrap",
                          nboot = 10)
  expect_identical(too_few_valid@between$table$n_boot_attempted, 10L)
  expect_identical(too_few_valid@between$table$n_boot_valid, 7L)
  expect_identical(too_few_valid@between$table$inference_status,
                   "unavailable")
  expect_identical(too_few_valid@between$table$inference_reason,
                   "fewer_than_ten_valid_bootstrap_replicates")
  point_estimate <- wbCorr(dat, "id", inference = "none")
  expect_equal(too_few_valid@between$correlations["x", "y"],
               point_estimate@between$correlations["x", "y"])
  expect_match(too_few_valid@between$table$warning,
               "7 of 10 cluster-bootstrap replicates")
  expect_true(is.na(too_few_valid@between$p_values["x", "y"]))
  expect_true(is.na(too_few_valid@between$confidence_intervals$CI_lower))
  expect_true(is.na(too_few_valid@between$confidence_intervals$CI_upper))

  two_clusters <- dat[dat$id < 3, ]
  skipped <- wbCorr(two_clusters,
                    "id",
                    inference = "cluster_bootstrap",
                    nboot = 10)
  expect_identical(skipped@between$table$status, "ok")
  expect_identical(skipped@between$table$n_boot_attempted, 0L)
  expect_identical(skipped@between$table$n_boot_valid, 0L)
  expect_identical(skipped@between$table$inference_status, "unavailable")
  expect_identical(skipped@between$table$inference_reason,
                   "fewer_than_three_clusters_for_bootstrap")
})
