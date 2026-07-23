test_that("numeric matrix accessors return stored unrounded coefficients", {
  data("simdat_intensive_longitudinal")
  result <- suppressWarnings(
    wbCorr(simdat_intensive_longitudinal, "participantID")
  )

  numeric_matrices <- get_matrix(result, numeric = TRUE)
  expect_equal(numeric_matrices$within,
               result@within$correlations,
               tolerance = 0)
  expect_equal(numeric_matrices$between,
               result@between$correlations,
               tolerance = 0)
  expect_true(all(vapply(numeric_matrices$within,
                         is.numeric,
                         logical(1))))
  expect_true(all(vapply(numeric_matrices$between,
                         is.numeric,
                         logical(1))))

  within_values <- as.matrix(numeric_matrices$within)
  off_diagonal <- within_values[upper.tri(within_values)]
  expect_true(any(abs(off_diagonal - round(off_diagonal, 2)) > 1e-8,
                  na.rm = TRUE))

  formatted <- get_matrix(result)
  expect_true(all(vapply(formatted$within, is.character, logical(1))))
  expect_true("note" %in% names(formatted))
  expect_false("note" %in% names(numeric_matrices))
})


test_that("numeric merged matrices combine levels and unrounded ICCs", {
  data("simdat_intensive_longitudinal")
  result <- suppressWarnings(
    wbCorr(simdat_intensive_longitudinal, "participantID")
  )
  matrices <- get_matrix(result, numeric = TRUE)

  within <- as.matrix(result@within$correlations)
  between <- as.matrix(result@between$correlations)
  merged_wb <- as.matrix(matrices$merged_wb)
  merged_bw <- as.matrix(matrices$merged_bw)

  expect_type(merged_wb, "double")
  expect_equal(merged_wb[upper.tri(merged_wb)],
               within[upper.tri(within)],
               tolerance = 0)
  expect_equal(merged_wb[lower.tri(merged_wb)],
               between[lower.tri(between)],
               tolerance = 0)
  expect_equal(merged_bw[upper.tri(merged_bw)],
               between[upper.tri(between)],
               tolerance = 0)
  expect_equal(merged_bw[lower.tri(merged_bw)],
               within[lower.tri(within)],
               tolerance = 0)

  expected_icc <- result@ICC$ICC[
    match(rownames(merged_wb), result@ICC$variable)
  ]
  expect_equal(unname(diag(merged_wb)), expected_icc, tolerance = 0)
  expect_equal(unname(diag(merged_bw)), expected_icc, tolerance = 0)
})


test_that("numeric access is available through aliases and summary", {
  dat <- data.frame(
    id = rep(1:4, each = 3),
    x = c(1, 4, 2, 3, 7, 5, 6, 8, 11, 9, 10, 14),
    y = c(3, 1, 5, 7, 2, 6, 8, 4, 10, 9, 13, 11)
  )
  result <- wbCorr(dat, "id", inference = "none")

  direct <- get_matrix(result, which = "within", numeric = TRUE)
  alias <- get_matrices(result, which = "within", numeric = TRUE)
  via_summary <- summary(result, which = "within", numeric = TRUE)
  expect_equal(alias, direct, tolerance = 0)
  expect_equal(via_summary, direct, tolerance = 0)

  expect_error(get_matrix(result, numeric = NA), "numeric")
  expect_error(get_matrix(result, numeric = c(TRUE, FALSE)), "numeric")
  expect_error(get_matrix(result, numeric = 1), "numeric")
})
