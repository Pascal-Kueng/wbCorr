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
