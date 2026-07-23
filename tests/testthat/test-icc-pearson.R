test_that("ICC(1,1) matches the balanced one-way random-effects estimator", {
  dat <- data.frame(
    id = rep(1:3, each = 2),
    x = 1:6,
    y = 6:1
  )

  cors <- wbCorr(dat, cluster = "id", inference = "none")
  icc <- get_ICC(cors)

  expect_named(icc, c("variable", "ICC"))
  expect_equal(icc$variable, c("x", "y"))
  expect_equal(icc$ICC, c(15 / 17, 15 / 17))
})


test_that("ICC(1,1) handles unequal clusters and variable missingness", {
  x <- c(0, 2, 4, 6, 8, 9, 11, 13, 15)
  x_missing <- x
  x_missing[9] <- NA_real_
  dat <- data.frame(
    id = rep(1:3, times = c(2, 3, 4)),
    x = x,
    x_missing = x_missing
  )

  equal <- wbCorr(dat, cluster = "id", inference = "none")
  weighted <- wbCorr(dat,
                     cluster = "id",
                     inference = "none",
                     between_weighting = "cluster_size")
  icc <- get_ICC(equal)

  expect_equal(icc$ICC[icc$variable == "x"], 73 / 86)
  expect_equal(icc$ICC[icc$variable == "x_missing"], 1529 / 1781)
  expect_equal(get_ICC(weighted), icc)
})


test_that("ICC(1,1) retains negative and boundary estimates", {
  dat <- data.frame(
    id = rep(1:3, each = 2),
    negative = rep(c(-1, 1), 3),
    one = rep(1:3, each = 2),
    constant = rep(1, 6)
  )

  icc <- get_ICC(wbCorr(dat, cluster = "id", inference = "none"))

  expect_equal(icc$ICC[icc$variable == "negative"], -1)
  expect_equal(icc$ICC[icc$variable == "one"], 1)
  expect_true(is.na(icc$ICC[icc$variable == "constant"]))

  # The unconstrained unequal-size ANOVA estimator can be below -1.
  severely_unbalanced <- data.frame(
    id = c(1, 2, 2),
    x = c(0, -1, 1),
    y = c(0, 1, -1)
  )
  severely_unbalanced_icc <- get_ICC(
    wbCorr(severely_unbalanced, cluster = "id", inference = "none")
  )
  expect_equal(severely_unbalanced_icc$ICC, c(-3, -3))

  singleton_dat <- data.frame(id = 1:3, x = 1:3, y = 3:1)
  one_cluster_dat <- data.frame(id = rep(1, 3), x = 1:3, y = 3:1)

  expect_true(all(is.na(get_ICC(wbCorr(singleton_dat,
                                       cluster = "id",
                                       inference = "none"))$ICC)))
  expect_true(all(is.na(get_ICC(wbCorr(one_cluster_dat,
                                       cluster = "id",
                                       inference = "none"))$ICC)))

  compute_icc1 <- getFromNamespace("compute_icc1", "wbCorr")
  clean_values <- data.frame(x = 1:6)
  clean_clusters <- rep(1:3, each = 2)
  dirty_values <- data.frame(x = c(1:6, Inf, -Inf, 999))
  dirty_clusters <- c(clean_clusters, 4, 4, NA)

  expect_equal(compute_icc1(dirty_values, dirty_clusters),
               compute_icc1(clean_values, clean_clusters))
})


test_that("Pearson analytic results match stats::cor.test", {
  x <- 1:12
  y <- c(2, 1, 4, 3, 5, 7, 6, 9, 8, 10, 12, 11)
  reference <- cor.test(x, y, method = "pearson", conf.level = 0.95)
  pearson_from_r <- getFromNamespace("cor_pearson_from_r", "wbCorr")

  result <- pearson_from_r(cor(x, y), length(x) - 2, 0.95)

  expect_equal(result$correlation_coefficient,
               unname(reference$estimate))
  expect_equal(result$test_statistic,
               unname(reference$statistic))
  expect_equal(result$p_value, reference$p.value)
  expect_equal(c(result$lower_bound, result$upper_bound),
               as.numeric(reference$conf.int))
  expect_true(result$lower_bound >= -1)
  expect_true(result$upper_bound <= 1)
})


test_that("within and between Pearson intervals use their adjusted samples", {
  id <- rep(1:8, each = 3)
  x <- rep(1:8, each = 3) + rep(c(-1, 0, 1), 8)
  y <- rep(c(1, 2, 3, 4, 5, 6, 7, 9), each = 3) +
    rep(c(-1, 0.25, 0.75), 8)
  dat <- data.frame(id = id, x = x, y = y)

  cors <- suppressWarnings(wbCorr(dat, cluster = "id"))

  means <- aggregate(cbind(x, y) ~ id, data = dat, FUN = mean)
  between_reference <- cor.test(means$x, means$y)
  between_ci <- cors@between$confidence_intervals[1, ]

  expect_equal(cors@between$correlations["x", "y"],
               unname(between_reference$estimate))
  expect_equal(cors@between$p_values["x", "y"],
               between_reference$p.value)
  expect_equal(c(between_ci$CI_lower, between_ci$CI_upper),
               as.numeric(between_reference$conf.int))

  x_residual <- x - ave(x, id, FUN = mean)
  y_residual <- y - ave(y, id, FUN = mean)
  within_r <- cor(x_residual, y_residual)
  within_df <- length(x) - length(unique(id)) - 1
  critical_value <- qnorm(0.975)
  expected_within_ci <- tanh(
    atanh(within_r) + c(-1, 1) * critical_value / sqrt(within_df - 1)
  )
  within_ci <- cors@within$confidence_intervals[1, ]

  expect_equal(cors@within$correlations["x", "y"], within_r)
  expect_equal(c(within_ci$CI_lower, within_ci$CI_upper),
               expected_within_ci)
})


test_that("Pearson keeps coefficients when analytic inference is unavailable", {
  pearson_from_r <- getFromNamespace("cor_pearson_from_r", "wbCorr")

  no_inference <- pearson_from_r(0.5, 0, 0.95)
  expect_equal(no_inference$correlation_coefficient, 0.5)
  expect_true(all(is.na(unlist(no_inference[-1]))))

  test_only <- pearson_from_r(0.5, 1, 0.95)
  expect_equal(test_only$correlation_coefficient, 0.5)
  expect_false(is.na(test_only$test_statistic))
  expect_false(is.na(test_only$p_value))
  expect_true(is.na(test_only$lower_bound))
  expect_true(is.na(test_only$upper_bound))

  perfect <- pearson_from_r(-1, 5, 0.95)
  expect_identical(perfect$test_statistic, -Inf)
  expect_equal(perfect$p_value, 0)
  expect_equal(c(perfect$lower_bound, perfect$upper_bound), c(-1, -1))

  dat <- data.frame(
    id = rep(1:3, each = 2),
    x = rep(1:3, each = 2) + rep(c(-0.25, 0.25), 3),
    y = rep(c(1, 3, 2), each = 2) + rep(c(-0.5, 0.5), 3)
  )
  analytic <- suppressWarnings(wbCorr(dat, cluster = "id"))
  point_only <- wbCorr(dat, cluster = "id", inference = "none")

  expect_equal(analytic@between$correlations,
               point_only@between$correlations)
  expect_false(is.na(analytic@between$p_values["x", "y"]))
  expect_true(is.na(analytic@between$confidence_intervals$CI_lower[1]))
  expect_true(is.na(analytic@between$confidence_intervals$CI_upper[1]))
  expect_identical(analytic@between$table$inference_status, "partial")
  expect_identical(analytic@between$table$inference_reason,
                   "confidence_interval_unavailable_low_df")
})


test_that("weighted between Pearson never uses unweighted analytic inference", {
  dat <- data.frame(
    id = rep(1:5, times = c(2, 3, 4, 5, 8)),
    x = rep(c(1, 2, 4, 7, 11), times = c(2, 3, 4, 5, 8)),
    y = rep(c(2, 5, 3, 9, 8), times = c(2, 3, 4, 5, 8))
  )

  emitted_warnings <- character()
  weighted <- withCallingHandlers(
    wbCorr(dat,
           cluster = "id",
           between_weighting = "cluster_size"),
    warning = function(warning_condition) {
      emitted_warnings <<- c(emitted_warnings,
                             conditionMessage(warning_condition))
      invokeRestart("muffleWarning")
    }
  )

  expect_true(any(grepl("Analytic inference is not supported for cluster-size-weighted",
                        emitted_warnings,
                        fixed = TRUE)))
  expect_equal(weighted@settings$between_inference, "none")
  expect_false(is.na(weighted@between$correlations["x", "y"]))
  expect_true(is.na(weighted@between$p_values["x", "y"]))
  expect_true(all(is.na(weighted@between$confidence_intervals[, c("CI_lower", "CI_upper")])))
  expect_match(weighted@between$table$warning[1],
               "weighted between analytic inference unavailable")
})
