dat <- data.frame(
  id = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
  x = c(1, 2, 100, 2, 4, 6, 3, NA, 9, 5, 1, 2),
  y = c(1, NA, 3, 2, 4, 8, NA, 8, 12, 4, 4, 1)
)
pair <- complete.cases(dat$x, dat$y)
x_means <- tapply(dat$x, dat$id, mean, na.rm = TRUE)
y_means <- tapply(dat$y, dat$id, mean, na.rm = TRUE)

test_that("cluster means use all available observations of each variable", {
  cors <- wbCorr(dat, cluster = "id")

  x_within <- dat$x - x_means[as.character(dat$id)]
  y_within <- dat$y - y_means[as.character(dat$id)]

  expect_equal(cors@within$correlations["x", "y"],
               cor(x_within[pair], y_within[pair]))
  expect_equal(cors@within$table$df,
               sum(pair) - length(unique(dat$id[pair])) - 1)
  expect_equal(cors@between$correlations["x", "y"],
               cor(x_means, y_means))
  expect_equal(cors@between$table$df, length(x_means) - 2)
})

test_that("cluster_size weighting weights cluster means by complete pairs", {
  weighted <- wbCorr(dat, cluster = "id", between_weighting = "cluster_size")

  weights <- as.numeric(table(dat$id[pair]))
  expected <- cov.wt(cbind(x_means, y_means), wt = weights, cor = TRUE)$cor[1, 2]

  expect_equal(weighted@between$correlations["x", "y"], expected)
  expect_equal(weighted@between$table$df, length(weights) - 2)
})

test_that("between correlations need at least three clusters", {
  two_clusters <- data.frame(id = rep(1:2, each = 3),
                             x = c(1, 3, 2, 5, 4, 6),
                             y = c(2, 1, 3, 6, 4, 5))
  cors <- wbCorr(two_clusters, cluster = "id")

  expect_true(is.na(cors@between$correlations["x", "y"]))
  expect_equal(nrow(cors@between$table), 1)
  expect_false(is.na(cors@within$correlations["x", "y"]))
})

test_that("the cluster column is excluded and rows without cluster are dropped", {
  with_missing_cluster <- rbind(dat, data.frame(id = NA, x = 50, y = -50))

  expect_equal(wbCorr(with_missing_cluster, "id")@within$correlations,
               wbCorr(dat, "id")@within$correlations)
  expect_equal(colnames(wbCorr(dat, dat$id)@within$correlations), c("x", "y"))
})

test_that("auto uses Spearman for ordered factors", {
  set.seed(1)
  ordinal <- data.frame(
    id = rep(1:10, each = 5),
    x = rnorm(50),
    rating = factor(sample(c("low", "mid", "high"), 50, replace = TRUE),
                    levels = c("low", "mid", "high"), ordered = TRUE)
  )

  expect_warning(cors <- wbCorr(ordinal, "id", method = "auto"),
                 "non-binary ordinal")
  expect_equal(cors@within$table$method, "spearman's rho")
})
