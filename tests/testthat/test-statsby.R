test_that("complete-data correlations agree with psych::statsBy", {
  testthat::skip_if_not_installed("psych")

  data("simdat_intensive_longitudinal")
  variables <- c("var1", "var2", "var3")
  participant_ids <- levels(simdat_intensive_longitudinal$participantID)[1:10]
  dat <- droplevels(
    simdat_intensive_longitudinal[
      simdat_intensive_longitudinal$participantID %in% participant_ids,
      c("participantID", variables)
    ]
  )

  expect_length(unique(table(dat$participantID)), 1L)
  expect_false(anyNA(dat))

  ours <- wbCorr(dat, cluster = "participantID", inference = "none")
  theirs <- suppressWarnings(
    psych::statsBy(
      dat,
      group = "participantID",
      cors = TRUE,
      method = "pearson"
    )
  )

  psych_within_names <- paste0(variables, ".wg")
  psych_between_names <- paste0(variables, ".bg")

  expect_equal(
    unname(as.matrix(ours@within$correlations[variables, variables])),
    unname(theirs$rwg[psych_within_names, psych_within_names]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(as.matrix(ours@between$correlations[variables, variables])),
    unname(theirs$rbg[psych_between_names, psych_between_names]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(ours@ICC$ICC),
    unname(theirs$ICC1[variables]),
    tolerance = 1e-10
  )
})


test_that("unbalanced data agree with psych::statsBy", {
  testthat::skip_if_not_installed("psych")

  set.seed(3)
  dat <- data.frame(id = rep(1:25, times = sample(2:8, 25, replace = TRUE)))
  dat$x <- rnorm(nrow(dat)) + rnorm(25)[dat$id]
  dat$y <- 0.3 * dat$x + rnorm(nrow(dat))
  theirs <- suppressWarnings(psych::statsBy(dat, group = "id", cors = TRUE))

  # psych::statsBy weights between-cluster correlations by cluster size.
  ours <- wbCorr(dat, cluster = "id", inference = "none",
                 between_weighting = "cluster_size")
  expect_equal(ours@within$correlations["x", "y"], theirs$rwg["x.wg", "y.wg"])
  expect_equal(ours@between$correlations["x", "y"], theirs$rbg["x.bg", "y.bg"])

  # Within-cluster deviations also agree when data are missing.
  dat$x[c(3, 10, 40)] <- NA
  dat$y[c(5, 10, 60)] <- NA
  theirs <- suppressWarnings(psych::statsBy(dat, group = "id", cors = TRUE))
  ours <- wbCorr(dat, cluster = "id", inference = "none")
  expect_equal(ours@within$correlations["x", "y"], theirs$rwg["x.wg", "y.wg"])
})
