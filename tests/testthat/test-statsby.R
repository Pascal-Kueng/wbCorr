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

  ours <- suppressWarnings(
    wbCorr(
      dat,
      cluster = "participantID",
      inference = "none",
      between_weighting = "equal_clusters"
    )
  )
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


test_that("balanced three-level correlations agree with recursive psych::statsBy", {
  testthat::skip_if_not_installed("psych")

  design <- expand.grid(
    occasion = seq_len(5L),
    person = seq_len(18L)
  )
  design$dyad <- rep(seq_len(6L), each = 15L)
  set.seed(302)
  person_x <- rnorm(18L)
  person_y <- -0.4 * person_x + rnorm(18L)
  dyad_x <- rnorm(6L)
  dyad_y <- 0.7 * dyad_x + rnorm(6L)
  occasion_x <- rnorm(nrow(design))
  occasion_y <- 0.5 * occasion_x + rnorm(nrow(design))
  dat <- data.frame(
    person = design$person,
    dyad = design$dyad,
    x = rep(person_x, each = 5L) +
      rep(dyad_x, each = 15L) +
      occasion_x,
    y = rep(person_y, each = 5L) +
      rep(dyad_y, each = 15L) +
      occasion_y
  )

  ours <- wbCorr(
    dat,
    cluster = list(person = "person", dyad = "dyad"),
    inference = "none"
  )
  by_person <- psych::statsBy(
    dat[, c("person", "x", "y")],
    group = "person",
    cors = TRUE,
    method = "pearson"
  )
  person_means <- data.frame(
    dyad = rep(seq_len(6L), each = 3L),
    x = by_person$mean[, "x"],
    y = by_person$mean[, "y"]
  )
  by_dyad <- psych::statsBy(
    person_means,
    group = "dyad",
    cors = TRUE,
    method = "pearson"
  )

  matrices <- get_matrix(ours, numeric = TRUE)
  expect_equal(
    unname(as.matrix(matrices$level1[c("x", "y"), c("x", "y")])),
    unname(by_person$rwg[c("x.wg", "y.wg"), c("x.wg", "y.wg")]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(as.matrix(matrices$level2[c("x", "y"), c("x", "y")])),
    unname(by_dyad$rwg[c("x.wg", "y.wg"), c("x.wg", "y.wg")]),
    tolerance = 1e-10
  )
  expect_equal(
    unname(as.matrix(matrices$level3[c("x", "y"), c("x", "y")])),
    unname(by_dyad$rbg[c("x.bg", "y.bg"), c("x.bg", "y.bg")]),
    tolerance = 1e-10
  )
})
