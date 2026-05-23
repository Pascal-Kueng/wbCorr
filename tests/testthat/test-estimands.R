test_that("within correlations use pair-specific centering and df", {
  dat <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    x = c(1, 2, 100, 2, 4, 6, 3, NA, 9),
    y = c(1, NA, 3, 2, 4, 8, NA, 8, 12)
  )

  cors <- wbCorr(dat, cluster = "id")

  valid <- complete.cases(dat$x, dat$y, dat$id)
  pair <- dat[valid, ]
  x_resid <- pair$x - ave(pair$x, pair$id, FUN = mean)
  y_resid <- pair$y - ave(pair$y, pair$id, FUN = mean)
  expected_r <- cor(x_resid, y_resid)
  expected_df <- nrow(pair) - length(unique(pair$id)) - 1

  expect_equal(cors@within$correlations["x", "y"], expected_r)
  expect_equal(cors@within$table$df, expected_df)
  expect_false("id" %in% colnames(cors@within$correlations))
})

test_that("between weighting is explicit and based on pair-specific cluster means", {
  dat <- data.frame(
    id = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    x = c(1, 2, 100, 2, 4, 6, 3, NA, 9),
    y = c(1, NA, 3, 2, 4, 8, NA, 8, 12)
  )

  equal <- wbCorr(dat, cluster = "id")
  weighted <- wbCorr(dat, cluster = "id",
                     between_weighting = "cluster_size")
  alias <- wbCorr(dat, cluster = "id",
                  weighted_between_statistics = TRUE)

  valid <- complete.cases(dat$x, dat$y, dat$id)
  pair <- dat[valid, ]
  means <- aggregate(cbind(x, y) ~ id, data = pair, FUN = mean)
  cluster_n <- aggregate(x ~ id, data = pair, FUN = length)
  weights <- cluster_n$x
  weighted_mean_x <- sum(weights * means$x) / sum(weights)
  weighted_mean_y <- sum(weights * means$y) / sum(weights)
  expected_weighted <- sum(weights * (means$x - weighted_mean_x) *
                             (means$y - weighted_mean_y)) /
    sqrt(sum(weights * (means$x - weighted_mean_x)^2) *
           sum(weights * (means$y - weighted_mean_y)^2))

  expect_equal(equal@between$correlations["x", "y"], cor(means$x, means$y))
  expect_equal(weighted@between$correlations["x", "y"], expected_weighted)
  expect_equal(alias@between$correlations, weighted@between$correlations)
  expect_equal(equal@between$table$df, nrow(means) - 2)
  expect_match(weighted@between$table$warning,
               "weighted between inference approximate")
})
