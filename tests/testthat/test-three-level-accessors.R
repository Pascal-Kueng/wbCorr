three_level_accessor_section <- function(level, correlation, p_value) {
  variables <- c("x", "y")
  correlations <- matrix(
    c(1, correlation, correlation, 1),
    nrow = 2,
    dimnames = list(variables, variables)
  )
  p_values <- matrix(
    c(NA, p_value, p_value, NA),
    nrow = 2,
    dimnames = list(variables, variables)
  )

  list(
    correlations = as.data.frame(correlations),
    p_values = as.data.frame(p_values),
    confidence_intervals = matrix(
      NA_real_,
      nrow = 2,
      ncol = 2,
      dimnames = list(variables, variables)
    ),
    table = data.frame(level = level, correlation = correlation),
    matrix_diagnostics = data.frame(
      level = level,
      status = "positive_semidefinite",
      is_complete = TRUE,
      is_psd = TRUE,
      min_eigenvalue = 1 - abs(correlation),
      tolerance = sqrt(.Machine$double.eps),
      n_variables = 2L,
      missing_data = "listwise",
      guaranteed_by_construction = TRUE,
      reason = NA_character_,
      stringsAsFactors = FALSE
    )
  )
}


three_level_accessor_object <- function() {
  levels <- list(
    level1 = three_level_accessor_section("level1", 0.1234, 0.004),
    level2 = three_level_accessor_section("level2", -0.4321, 0.03),
    level3 = three_level_accessor_section("level3", 0.2468, 0.4)
  )

  methods::new(
    "wbCorrNested",
    within = levels$level1,
    between = levels$level3,
    ICC = data.frame(variable = c("x", "y"), ICC = c(0.2, 0.3)),
    centered_data = list(),
    settings = list(),
    levels = levels
  )
}


test_that("nested table accessors default to all levels and normalize aliases", {
  object <- three_level_accessor_object()

  tables <- get_table(object)
  expect_named(tables, c("level1", "level2", "level3"))
  expect_equal(tables$level1, object@levels$level1$table)
  expect_equal(tables$level2, object@levels$level2$table)
  expect_equal(tables$level3, object@levels$level3$table)

  selected <- get_tables(object, which = c("l3", "level1", "l1"))
  expect_named(selected, c("level3", "level1"))
  expect_equal(selected$level3, object@levels$level3$table)
  expect_named(
    get_table(object, which = NULL),
    c("level1", "level2", "level3")
  )
})


test_that("nested matrix accessors return formatted or numeric level matrices", {
  object <- three_level_accessor_object()

  formatted <- get_matrix(object)
  expect_equal(names(formatted)[seq_len(3)],
               c("level1", "level2", "level3"))
  expect_identical(formatted$level1["x", "y"], "0.12**")
  expect_identical(formatted$level2["x", "y"], "-0.43*")
  expect_identical(
    formatted$note,
    "***p < 0.001, **p < 0.01, *p < 0.05"
  )

  numeric <- get_matrices(object, which = c("l2", "l3"), numeric = TRUE)
  expect_named(numeric, c("level2", "level3"))
  expect_equal(numeric$level2, object@levels$level2$correlations,
               tolerance = 0)
  expect_equal(numeric$level3, object@levels$level3$correlations,
               tolerance = 0)
  expect_true(all(vapply(numeric, function(x) {
    all(vapply(x, is.numeric, logical(1)))
  }, logical(1))))

  via_summary <- summary(object, which = "l1", numeric = TRUE)
  expect_named(via_summary, "level1")
  expect_equal(via_summary$level1, object@levels$level1$correlations,
               tolerance = 0)
  expect_named(
    get_matrix(object, which = NULL),
    c("level1", "level2", "level3", "note")
  )
})


test_that("nested accessors reject merged matrices and ICC requests clearly", {
  object <- three_level_accessor_object()

  for (selector in c("merge", "m", "merged", "merge_bw", "bw",
                     "merge_wb", "wb")) {
    expect_error(
      get_matrix(object, which = selector),
      "Merged matrices are not available for wbCorrNested"
    )
  }

  expect_error(get_table(object, which = "merge"),
               "Merged matrices are not available for wbCorrNested")
  expect_error(get_ICC(object), "ICCs are not supported for wbCorrNested")
  expect_error(get_ICCs(object), "ICCs are not supported for wbCorrNested")
  expect_error(get_icc(object), "ICCs are not supported for wbCorrNested")
})


test_that("nested matrix diagnostics support level selectors", {
  object <- three_level_accessor_object()

  diagnostics <- get_matrix_diagnostics(object)
  expect_equal(diagnostics$level, c("level1", "level2", "level3"))
  expect_true(all(diagnostics$is_psd))

  selected <- get_matrix_diagnostics(object, which = c("l3", "l1"))
  expect_equal(selected$level, c("level3", "level1"))
  expect_equal(selected$min_eigenvalue,
               c(1 - 0.2468, 1 - 0.1234))
})
