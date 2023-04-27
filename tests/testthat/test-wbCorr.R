

# on my simulated data
data("simdat_intensive_longitudinal")
exp_tables <- readRDS("testdata/exp_tables.rds")
exp_matrices <- readRDS("testdata/exp_matrices.rds")

test_that("pearson stats are computed correctly weighted and unweighted if all observations have the same amount of missings", {
  simdat_intensive_longitudinal$participantID <- as.numeric(simdat_intensive_longitudinal$participantID)

  cors_weighted <- wbCorr(simdat_intensive_longitudinal,
                          cluster = 'participantID',
                          weighted_between_statistics = TRUE)
  cors_not_weighted <- wbCorr(simdat_intensive_longitudinal,
                              cluster = 'participantID',
                              weighted_between_statistics = FALSE)

  expect_equal(get_tables(cors_weighted), exp_tables)
  expect_equal(summary(cors_weighted), exp_matrices)
  expect_equal(summary(cors_weighted), exp_matrices)
  expect_equal(get_table(cors_weighted), exp_tables)
})



test_that("correlations are equal to statsBy implementation", {
  # function to compare coefficients!
  compare <- function(cors_weighted, statsby) {
    # between correlation
    df_statsby <- round(as.data.frame(statsby$rbg), 8)
    df_wbcorr <- round(cors_weighted@between$correlations, 8)

    compare_between <- sum(df_statsby == df_wbcorr, na.rm = TRUE)
    expect_equal(compare_between, 9)

    # between p-values
    df_statsby <- round(as.data.frame(statsby$pbg), 4)
    df_wbcorr <- round(cors_weighted@between$p_values, 4)
    print(df_statsby)
    print(df_wbcorr)

    compare_between <- sum(df_statsby == df_wbcorr, na.rm = TRUE)
    expect_equal(compare_between, 9)

    # within correlation
    df_statsby <- round(as.data.frame(statsby$rwg), 8)
    df_wbcorr <- round(cors_weighted@within$correlations, 8)

    compare_within <- sum(df_statsby == df_wbcorr, na.rm = TRUE)
    expect_equal(compare_within, 16)

    # within p-values
    df_statsby <- round(as.data.frame(statsby$pwg), 3)
    df_wbcorr <- round(cors_weighted@within$p_values, 3)
    compare_within <- sum(df_statsby == df_wbcorr, na.rm = TRUE)

    expect_equal(compare_within, 16)

    # within CIs
    df_statsby <- statsby$ci.wg[1]$r.ci
    df_statsby
    df_wbcorr <- cors_weighted@within$confidence_intervals
    df_wbcorr
  }


  # run with pearson.
  cors_weighted <- wbCorr(simdat_intensive_longitudinal,
                          cluster = 'participantID',
                          weighted_between_statistics = TRUE)

  statsby <- suppressWarnings(psych::statsBy(simdat_intensive_longitudinal,
                                             group = 'participantID'))

  compare(cors_weighted, statsby)

  # run with spearman.
  cors_weighted <- wbCorr(simdat_intensive_longitudinal,
                          cluster = 'participantID',
                          weighted_between_statistics = TRUE,
                          method = 'spearman')

  statsby <- suppressWarnings(psych::statsBy(simdat_intensive_longitudinal,
                                             group = 'participantID',
                                             method = 'spearman'))

  compare(cors_weighted, statsby)
})

# on other data


