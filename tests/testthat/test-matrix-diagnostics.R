test_that("pairwise non-PSD between matrices are detected from raw coefficients", {
  # Adapted from the canonical stats::cor() pairwise-complete example: the
  # three off-diagonal correlations are 0, 1, and 1, which is incompatible
  # with a positive-semidefinite 3 x 3 correlation matrix.
  dat <- data.frame(
    id = 1:5,
    x = c(-2, -1, 0, 1, 2),
    y = c(1.5, 2, 0, 1, 2),
    z = c(NA, NA, 0, 1, 2)
  )

  expect_warning(
    pairwise <- wbCorr(dat, "id", inference = "none"),
    "between-cluster correlation matrix is not positive semidefinite"
  )

  expected <- cor(dat[c("x", "y", "z")],
                  use = "pairwise.complete.obs")
  expect_equal(as.matrix(pairwise@between$correlations), expected)

  diagnostics <- get_matrix_diagnostics(pairwise)
  between <- diagnostics[diagnostics$level == "between", ]
  expect_identical(between$status, "non_positive_semidefinite")
  expect_identical(between$is_complete, TRUE)
  expect_identical(between$is_psd, FALSE)
  expect_lt(between$min_eigenvalue, -between$tolerance)
  expect_equal(between$min_eigenvalue, 1 - sqrt(2), tolerance = 1e-12)
  expect_identical(between$missing_data, "pairwise")
  expect_identical(between$guaranteed_by_construction, FALSE)
})


test_that("pairwise non-PSD within matrices are detected", {
  id <- rep(1:6, each = 2)
  wave <- rep(c(-1, 1), 6)
  dat <- data.frame(
    id = id,
    x = c(wave[1:4], rep(NA, 4), wave[9:12]),
    y = c(wave[1:8], rep(NA, 4)),
    z = c(rep(NA, 4), wave[5:8], -wave[9:12])
  )

  expect_warning(
    result <- wbCorr(dat, "id", inference = "none"),
    "within-cluster correlation matrix is not positive semidefinite"
  )

  expected <- matrix(c(1, 1, -1,
                       1, 1, 1,
                       -1, 1, 1),
                     nrow = 3,
                     dimnames = list(c("x", "y", "z"),
                                     c("x", "y", "z")))
  expect_equal(as.matrix(result@within$correlations), expected)
  within <- get_matrix_diagnostics(result)
  within <- within[within$level == "within", ]
  expect_identical(within$is_psd, FALSE)
  expect_equal(within$min_eigenvalue, -1, tolerance = 1e-12)
})


test_that("listwise handling uses one sample and produces a PSD Pearson matrix", {
  dat <- data.frame(
    id = 1:5,
    x = c(-2, -1, 0, 1, 2),
    y = c(1.5, 2, 0, 1, 2),
    z = c(NA, NA, 0, 1, 2)
  )

  expect_silent(
    listwise <- wbCorr(dat,
                       "id",
                       inference = "none",
                       missing_data = "listwise")
  )

  expected <- cor(dat[complete.cases(dat), c("x", "y", "z")])
  expect_equal(as.matrix(listwise@between$correlations), expected)
  expect_true(all(listwise@between$table$n_obs == 3L))
  expect_true(all(listwise@between$table$n_clusters == 3L))
  expect_identical(listwise@settings$missing_data, "listwise")

  diagnostics <- get_matrix_diagnostics(listwise)
  between <- diagnostics[diagnostics$level == "between", ]
  expect_identical(between$status, "positive_semidefinite")
  expect_identical(between$is_complete, TRUE)
  expect_identical(between$is_psd, TRUE)
  expect_gte(between$min_eigenvalue, -between$tolerance)
  expect_identical(between$guaranteed_by_construction, TRUE)
})


test_that("unavailable coefficients make PSD status explicitly unassessable", {
  dat <- data.frame(
    id = 1:4,
    x = c(1, 2, NA, NA),
    y = c(NA, 3, 4, NA),
    z = c(NA, NA, 5, 6)
  )

  result <- wbCorr(dat,
                   "id",
                   inference = "none",
                   missing_data = "listwise")
  diagnostics <- get_matrix_diagnostics(result)

  expect_true(all(diagnostics$status == "not_assessable"))
  expect_true(all(!diagnostics$is_complete))
  expect_true(all(is.na(diagnostics$is_psd)))
  expect_true(all(diagnostics$reason ==
                    "matrix_contains_missing_or_non_finite_entries"))
  expect_true(all(result@within$table$n_obs == 0L))
  expect_true(all(result@between$table$n_obs == 0L))
})


test_that("listwise matrix handling does not change variable-wise ICC samples", {
  dat <- data.frame(
    id = rep(1:5, each = 3),
    x = c(NA, 1:14),
    y = c(15:3, NA, 1),
    z = rep(c(-1, 0, 1), 5)
  )

  pairwise <- wbCorr(dat, "id", inference = "none")
  expect_warning(
    listwise <- wbCorr(dat,
                       "id",
                       inference = "none",
                       missing_data = "listwise",
                       centering_rows = "all_available"),
    "equivalent.*pairwise_complete"
  )

  expect_equal(listwise@ICC, pairwise@ICC)
  expect_identical(listwise@settings$centering_rows, "pairwise_complete")
})


test_that("listwise Pearson, Spearman, and common weights match manual matrices", {
  cluster_sizes <- 2:7
  id <- rep(seq_along(cluster_sizes), cluster_sizes)
  occasion <- sequence(cluster_sizes)
  dat <- data.frame(
    id = id,
    x = 2 * id + occasion,
    y = c(4, 1, 6, 2, 8, 3)[id] + occasion^2 / 5,
    z = id - 2 * occasion + (id * occasion) %% 3
  )
  dat$x[c(3, 17)] <- NA_real_
  dat$y[c(8, 24)] <- NA_real_
  dat$z[c(12, 26)] <- NA_real_

  filtered <- dat[complete.cases(dat), , drop = FALSE]
  residuals <- filtered[c("x", "y", "z")]
  residuals[] <- lapply(residuals, function(values) {
    values - ave(values, filtered$id, FUN = mean)
  })
  means <- aggregate(cbind(x, y, z) ~ id, filtered, mean)

  pearson <- wbCorr(dat,
                    "id",
                    inference = "none",
                    missing_data = "listwise")
  expect_equal(as.matrix(pearson@within$correlations), cor(residuals))
  expect_equal(as.matrix(pearson@between$correlations),
               cor(means[c("x", "y", "z")]))

  spearman <- wbCorr(dat,
                     "id",
                     method = "spearman",
                     inference = "none",
                     missing_data = "listwise")
  expect_equal(as.matrix(spearman@within$correlations),
               cor(residuals, method = "spearman"))
  expect_equal(as.matrix(spearman@between$correlations),
               cor(means[c("x", "y", "z")], method = "spearman"))

  weighted <- wbCorr(dat,
                     "id",
                     inference = "none",
                     between_weighting = "cluster_size",
                     missing_data = "listwise")
  weights <- as.numeric(table(factor(filtered$id,
                                     levels = means$id)))
  expected_weighted <- cov.wt(means[c("x", "y", "z")],
                              wt = weights,
                              cor = TRUE)$cor
  expect_equal(as.matrix(weighted@between$correlations), expected_weighted)
  expect_true(get_matrix_diagnostics(weighted)$is_psd[2])
})


test_that("listwise cluster bootstrap equals manual complete-row filtering", {
  id <- rep(1:7, each = 4)
  occasion <- rep(1:4, 7)
  dat <- data.frame(
    id = id,
    x = id + occasion + (id * occasion) %% 2,
    y = c(3, 1, 5, 2, 7, 4, 6)[id] + occasion / 3,
    z = 2 * id - occasion + (id + occasion) %% 3
  )
  dat$x[c(2, 15)] <- NA_real_
  dat$y[c(7, 22)] <- NA_real_
  dat$z[c(10, 27)] <- NA_real_
  filtered <- dat[complete.cases(dat), , drop = FALSE]

  set.seed(731)
  listwise <- wbCorr(dat,
                     "id",
                     inference = "cluster_bootstrap",
                     nboot = 20,
                     missing_data = "listwise")
  set.seed(731)
  manual <- wbCorr(filtered,
                   "id",
                   inference = "cluster_bootstrap",
                   nboot = 20)

  expect_equal(listwise@within$correlations, manual@within$correlations)
  expect_equal(listwise@between$correlations, manual@between$correlations)
  expect_equal(listwise@within$confidence_intervals,
               manual@within$confidence_intervals)
  expect_equal(listwise@between$confidence_intervals,
               manual@between$confidence_intervals)
  expect_equal(listwise@within$table$n_boot_valid,
               manual@within$table$n_boot_valid)
  expect_equal(listwise@between$table$n_boot_valid,
               manual@between$table$n_boot_valid)
})


test_that("missing-data mode and diagnostic accessor validate inputs", {
  dat <- data.frame(id = rep(1:3, each = 2), x = 1:6, y = 6:1)

  expect_error(wbCorr(dat, "id", missing_data = "complete"),
               "missing_data")
  expect_error(get_matrix_diagnostics(dat), "wbCorr object")
})
