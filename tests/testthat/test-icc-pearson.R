test_that("ICC(1,1) matches the balanced one-way random-effects estimator", {
  dat <- data.frame(
    id = rep(1:3, each = 2),
    x = 1:6,
    y = 6:1
  )

  icc <- get_ICC(wbCorr(dat, cluster = "id", inference = "none"))

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

  icc <- get_ICC(wbCorr(dat, cluster = "id", inference = "none"))

  expect_equal(icc$ICC[icc$variable == "x"], 73 / 86)
  expect_equal(icc$ICC[icc$variable == "x_missing"], 1529 / 1781)
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

  # The unequal-size ANOVA estimator can be below -1.
  severely_unbalanced <- data.frame(
    id = c(1, 2, 2),
    x = c(0, -1, 1),
    y = c(0, 1, -1)
  )
  expect_equal(get_ICC(wbCorr(severely_unbalanced, cluster = "id",
                              inference = "none"))$ICC, c(-3, -3))

  singletons <- data.frame(id = 1:3, x = 1:3, y = 3:1)
  one_cluster <- data.frame(id = rep(1, 3), x = 1:3, y = 3:1)
  expect_true(all(is.na(get_ICC(wbCorr(singletons, cluster = "id"))$ICC)))
  expect_true(all(is.na(get_ICC(wbCorr(one_cluster, cluster = "id"))$ICC)))
})


test_that("Pearson results match stats::cor.test", {
  x <- 1:12
  y <- c(2, 1, 4, 3, 5, 7, 6, 9, 8, 10, 12, 11)
  reference <- cor.test(x, y, method = "pearson", conf.level = 0.95)
  cor_pearson <- getFromNamespace("cor_pearson", "wbCorr")

  result <- cor_pearson(x, y, length(x) - 2, 0.95)

  expect_equal(result$correlation_coefficient, unname(reference$estimate))
  expect_equal(result$test_statistic, unname(reference$statistic))
  expect_equal(result$p_value, reference$p.value)
  expect_equal(c(result$lower_bound, result$upper_bound),
               as.numeric(reference$conf.int))
})


test_that("within and between intervals use their degrees of freedom", {
  id <- rep(1:8, each = 3)
  x <- rep(1:8, each = 3) + rep(c(-1, 0, 1), 8)
  y <- rep(c(1, 2, 3, 4, 5, 6, 7, 9), each = 3) + rep(c(-1, 0.25, 0.75), 8)
  dat <- data.frame(id = id, x = x, y = y)

  cors <- wbCorr(dat, cluster = "id")

  means <- aggregate(cbind(x, y) ~ id, data = dat, FUN = mean)
  between_reference <- cor.test(means$x, means$y)
  between_ci <- cors@between$confidence_intervals[1, ]

  expect_equal(cors@between$correlations["x", "y"],
               unname(between_reference$estimate))
  expect_equal(cors@between$p_values["x", "y"], between_reference$p.value)
  expect_equal(c(between_ci$CI_lower, between_ci$CI_upper),
               as.numeric(between_reference$conf.int))

  within_r <- cor(x - ave(x, id), y - ave(y, id))
  within_df <- length(x) - length(unique(id)) - 1
  expected_within_ci <- tanh(atanh(within_r) +
                               c(-1, 1) * qnorm(0.975) / sqrt(within_df - 1))
  within_ci <- cors@within$confidence_intervals[1, ]

  expect_equal(cors@within$correlations["x", "y"], within_r)
  expect_equal(c(within_ci$CI_lower, within_ci$CI_upper), expected_within_ci)
})


test_that("Spearman inference uses the Fisher z-transformation", {
  id <- rep(1:8, each = 3)
  x <- rep(1:8, each = 3) + rep(c(-1, 0, 1), 8)
  y <- rep(c(1, 2, 3, 4, 5, 6, 7, 9), each = 3) + rep(c(-1, 0.25, 0.75), 8)

  cors <- wbCorr(data.frame(id = id, x = x, y = y), cluster = "id",
                 method = "spearman")

  rho <- cors@between$correlations["x", "y"]
  se <- 1 / sqrt(8 - 3)
  expect_equal(cors@between$p_values["x", "y"],
               2 * pnorm(-abs(atanh(rho) / se)))
})
