validation_data <- function() {
  data.frame(
    id = rep(1:4, each = 3),
    x = c(1, 3, 2, 2, 5, 4, 4, 7, 8, 6, 9, 10),
    y = c(3, 1, 4, 4, 2, 6, 8, 5, 9, 7, 11, 10)
  )
}


test_that("scalar arguments are validated before estimation", {
  dat <- validation_data()

  invalid_confidence <- list(NULL, NA_real_, NaN, Inf, -Inf, 0, 1,
                             -0.1, 1.1, c(0.9, 0.95), "0.95", TRUE)
  for (value in invalid_confidence) {
    expect_error(
      wbCorr(dat, "id", confidence_level = value, inference = "none"),
      "confidence_level"
    )
  }

  invalid_nboot <- list(NULL, NA_real_, NaN, Inf, -Inf, -1, 0, 1, 9,
                        10.5, c(10, 20), "20", TRUE)
  for (value in invalid_nboot) {
    expect_error(
      wbCorr(dat, "id", nboot = value, inference = "none"),
      "nboot"
    )
  }

  valid <- wbCorr(dat, "id", nboot = 10, inference = "none")
  expect_identical(valid@settings$nboot, 10L)

  expect_error(wbCorr(dat, "id", method = c("pearson", "spearman")),
               "method")
  expect_error(wbCorr(dat, "id", method = NULL), "method")
  expect_error(wbCorr(dat, "id", bootstrap = NA), "bootstrap")
  expect_error(wbCorr(dat, "id", bootstrap = 1), "bootstrap")
  expect_error(wbCorr(dat, "id", weighted_between_statistics = NA),
               "weighted_between_statistics")
  expect_error(wbCorr(dat, "id", weighted_between_statistics = 1),
               "weighted_between_statistics")
})


test_that("choice arguments require exact scalar values", {
  dat <- validation_data()

  expect_error(wbCorr(dat, "id", inference = "n"), "inference")
  expect_error(wbCorr(dat, "id", inference = NULL), "inference")
  expect_error(wbCorr(dat, "id", between_weighting = "e"),
               "between_weighting")
  expect_error(wbCorr(dat, "id", between_inference = "n"),
               "between_inference")
  expect_error(wbCorr(dat, "id", centering_rows = "p"),
               "centering_rows")
})


test_that("cluster vectors have a strict shape and do not delete code-equivalent outcomes", {
  dat <- validation_data()[, c("x", "y")]
  cluster <- rep(1:4, each = 3)

  expect_error(wbCorr(dat, cluster[-1], inference = "none"),
               "cluster variable")
  expect_error(wbCorr(dat, c(cluster, 5), inference = "none"),
               "cluster variable")
  expect_error(wbCorr(dat, matrix(cluster, ncol = 1), inference = "none"),
               "cluster variable")
  expect_error(wbCorr(dat, as.list(cluster), inference = "none"),
               "cluster variable")
  expect_error(wbCorr(dat, rep(NA_integer_, nrow(dat)), inference = "none"),
               "at least one non-missing")
  expect_error(wbCorr(dat, replace(cluster, 1, Inf), inference = "none"),
               "not Inf")

  collision_data <- data.frame(
    cluster_level_outcome = rep(c(10, 20, 30, 40), each = 3),
    x = dat$x,
    y = dat$y
  )
  result <- wbCorr(collision_data, cluster, inference = "none")

  expect_true("cluster_level_outcome" %in%
                colnames(result@between$correlations))
  expect_equal(ncol(result@between$correlations), 3L)

  data_with_id <- validation_data()
  numeric_id <- wbCorr(data_with_id, as.numeric(data_with_id$id),
                       inference = "none")
  character_id <- wbCorr(data_with_id, as.character(data_with_id$id),
                         inference = "none")
  factor_id <- wbCorr(data_with_id, factor(data_with_id$id),
                      inference = "none")
  expect_identical(colnames(numeric_id@within$correlations), c("x", "y"))
  expect_identical(colnames(character_id@within$correlations), c("x", "y"))
  expect_identical(colnames(factor_id@within$correlations), c("x", "y"))

  near_data <- data_with_id
  near_data$near_id <- as.numeric(near_data$id)
  near_data$near_id[1] <- near_data$near_id[1] + .Machine$double.eps
  near_result <- wbCorr(near_data, as.numeric(near_data$id),
                        inference = "none")
  expect_false("id" %in% colnames(near_result@within$correlations))
  expect_true("near_id" %in% colnames(near_result@within$correlations))
})


test_that("named and supplied clusters receive identical content validation", {
  dat <- validation_data()

  all_missing <- dat
  all_missing$id <- NA_integer_
  expect_error(wbCorr(all_missing, "id", inference = "none"),
               "at least one non-missing")

  nonfinite <- dat
  nonfinite$id[1] <- Inf
  expect_error(wbCorr(nonfinite, "id", inference = "none"),
               "not Inf")
  nonfinite$id[1] <- NaN
  expect_error(wbCorr(nonfinite, "id", inference = "none"),
               "not Inf")

  matrix_cluster <- dat
  matrix_cluster$id <- I(matrix(dat$id, ncol = 1))
  expect_error(wbCorr(matrix_cluster, "id", inference = "none"),
               "atomic, dimensionless")

  list_cluster <- dat
  list_cluster$id <- I(as.list(dat$id))
  expect_error(wbCorr(list_cluster, "id", inference = "none"),
               "atomic, dimensionless")
})


test_that("non-finite outcomes are treated consistently as missing", {
  dat_na <- validation_data()
  dat_na$x[3] <- NA_real_
  dat_inf <- validation_data()
  dat_inf$x[3] <- Inf
  dat_negative_inf <- validation_data()
  dat_negative_inf$x[3] <- -Inf

  reference <- wbCorr(dat_na, "id", inference = "none")
  expect_warning(
    positive <- wbCorr(dat_inf, "id", inference = "none"),
    "treated as missing"
  )
  expect_warning(
    negative <- wbCorr(dat_negative_inf, "id", inference = "none"),
    "treated as missing"
  )

  expect_equal(positive@within$correlations, reference@within$correlations)
  expect_equal(positive@between$correlations, reference@between$correlations)
  expect_equal(positive@ICC, reference@ICC)
  expect_equal(negative@within$correlations, reference@within$correlations)
  expect_equal(negative@between$correlations, reference@between$correlations)
  expect_equal(negative@ICC, reference@ICC)

  both <- validation_data()
  both$x[1] <- Inf
  both$y[2] <- -Inf
  both_result <- suppressWarnings(wbCorr(both, "id", inference = "none"))
  expect_match(both_result@within$table$warning,
               "x non-finite values treated as missing")
  expect_match(both_result@within$table$warning,
               "y non-finite values treated as missing")
})


test_that("logical and two-level factor inputs match an explicit zero-one encoding", {
  id <- rep(1:6, each = 4)
  binary <- c(0, 0, 0, 1,
              0, 0, 1, 1,
              0, 1, 1, 1,
              0, 1, 0, 1,
              1, 0, 0, 0,
              1, 1, 0, 1)
  y <- c(1, 3, 2, 5, 2, 6, 5, 7, 4, 8, 7, 10,
         3, 9, 6, 11, 5, 4, 8, 9, 7, 12, 10, 14)

  numeric_data <- data.frame(id = id, binary = binary, y = y)
  logical_data <- data.frame(id = id, binary = as.logical(binary), y = y)
  factor_data <- data.frame(
    id = id,
    binary = factor(ifelse(binary == 1, "yes", "no"),
                    levels = c("no", "yes")),
    y = y
  )

  numeric_result <- wbCorr(numeric_data, "id", inference = "none")
  logical_result <- wbCorr(logical_data, "id", inference = "none")
  factor_result <- suppressWarnings(
    wbCorr(factor_data, "id", inference = "none")
  )

  expect_equal(logical_result@within$correlations,
               numeric_result@within$correlations)
  expect_equal(logical_result@between$correlations,
               numeric_result@between$correlations)
  expect_equal(logical_result@ICC, numeric_result@ICC)
  expect_equal(factor_result@within$correlations,
               numeric_result@within$correlations)
  expect_equal(factor_result@between$correlations,
               numeric_result@between$correlations)
  expect_equal(factor_result@ICC, numeric_result@ICC)
  expect_identical(factor_result@settings$var_type$binary, "binary")
  expect_true(is.numeric(factor_result@centered_data$within_df$binary))

  reversed_data <- factor_data
  reversed_data$binary <- factor(reversed_data$binary,
                                 levels = c("yes", "no"))
  reversed_result <- suppressWarnings(
    wbCorr(reversed_data, "id", inference = "none")
  )
  expect_equal(reversed_result@within$correlations["binary", "y"],
               -factor_result@within$correlations["binary", "y"])
  expect_equal(reversed_result@between$correlations["binary", "y"],
               -factor_result@between$correlations["binary", "y"])
  expect_equal(reversed_result@ICC, factor_result@ICC)
})


test_that("unsupported categorical inputs fail clearly and constant factors are retained", {
  dat <- validation_data()
  dat$character <- rep(c("no", "yes"), length.out = nrow(dat))
  expect_error(wbCorr(dat, "id", inference = "none"),
               "character.*explicit level order")

  dat <- validation_data()
  dat$nominal <- factor(rep(c("a", "b", "c"), length.out = nrow(dat)))
  expect_error(wbCorr(dat, "id", inference = "none"),
               "unordered factor.*dummy-code")

  dat <- validation_data()
  dat$ordinal <- ordered(rep(c("low", "middle", "high"),
                             length.out = nrow(dat)))
  expect_error(wbCorr(dat, "id", inference = "none"),
               "ordered factor.*meaningful numeric scores")

  dat <- validation_data()
  dat$constant_factor <- factor(rep("no", nrow(dat)),
                                levels = c("no", "yes"))
  dat$missing_factor <- factor(rep(NA_character_, nrow(dat)),
                               levels = c("no", "yes"))
  result <- suppressWarnings(
    wbCorr(dat, "id", method = "auto", inference = "none")
  )

  expect_true(all(c("constant_factor", "missing_factor") %in%
                    colnames(result@within$correlations)))
  expect_true(is.na(result@within$correlations["constant_factor",
                                                "constant_factor"]))
  expect_true(is.na(result@within$correlations["missing_factor",
                                                "missing_factor"]))
  expect_true(all(is.na(result@ICC$ICC[
    result@ICC$variable %in% c("constant_factor", "missing_factor")
  ])))

  normalize <- getFromNamespace("check_assumptions", "wbCorr")
  second_level_only <- factor(rep("yes", nrow(dat)),
                              levels = c("no", "yes"))
  normalized <- suppressWarnings(normalize(second_level_only,
                                            "second_level_only",
                                            "pearson"))
  expect_identical(normalized$col, rep(1, nrow(dat)))
  expect_identical(normalized$type, "binary")

  unused_multilevel <- validation_data()
  unused_multilevel$category <- factor(
    rep(c("low", "high"), length.out = nrow(unused_multilevel)),
    levels = c("low", "middle", "high")
  )
  expect_error(wbCorr(unused_multilevel, "id", inference = "none"),
               "does not declare exactly two levels")
})
