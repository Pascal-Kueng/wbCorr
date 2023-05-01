

# on my simulated data
data("simdat_intensive_longitudinal")
exp_tables_pearson_simdat <- readRDS("testdata/exp_tables_pearson_simdat.rds")
exp_matrices_pearson_simdat <- readRDS("testdata/exp_matrices_pearson_simdat.rds")
exp_tables_spearman_simdat <- readRDS("testdata/exp_tables_spearman_simdat.rds")
exp_matrices_spearman_simdat <- readRDS("testdata/exp_matrices_spearman_simdat.rds")
exp_tables_jackknife_simdat <- readRDS("testdata/exp_tables_jackknife_simdat.rds")
exp_matrices_jackknife_simdat <- readRDS("testdata/exp_matrices_jackknife_simdat.rds")

compare_to_expected_output <- function(cors, exp_matrices, exp_tables) {
  expect_equal(summary(cors), exp_matrices)
  expect_equal(get_table(cors), exp_tables)
}


test_that("all methods are corredct for simdat", {

  # Test pearson
  cors_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                          cluster = "participantID",
                          weighted_between_statistics = TRUE))
  cors_not_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                              cluster = "participantID",
                              weighted_between_statistics = FALSE))
  compare_to_expected_output(cors_not_weighted, exp_matrices_pearson_simdat, exp_tables_pearson_simdat)
  # Test spearman
  cors_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                          cluster = "participantID",
                          method = 'spearman',
                          weighted_between_statistics = TRUE))
  cors_not_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                              cluster = "participantID",
                              method = 'spearman',
                              weighted_between_statistics = FALSE))
  compare_to_expected_output(cors_not_weighted, exp_matrices_spearman_simdat, exp_tables_spearman_simdat)
  # Test jackknife

  cors_not_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                          cluster = 'participantID',
                          method = 'spearman-jackknife'))
  compare_to_expected_output(cors_not_weighted, exp_matrices_jackknife_simdat, exp_tables_jackknife_simdat)

  })


"
test_that('correlations are equal to statsBy implementation on simdat', {
  # function to compare coefficients!
  compare <- function(cors_weighted, statsby) {
    # between correlation
    df_statsby <- round(as.data.frame(statsby$rbg), 8)
    df_wbcorr <- round(cors_weighted@between$correlations, 8)

    compare_between <- sum(df_statsby == df_wbcorr, na.rm = TRUE)
    expect_equal(compare_between, 9)

    # between p-values
    df_statsby <- round(as.data.frame(statsby$pbg), 8)
    df_wbcorr <- round(cors_weighted@between$p_values, 8)
    difference <- df_statsby - df_wbcorr
    is.different <- abs(difference) < 0.001

    expect_equal(sum(is.different, na.rm = TRUE), 9)

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
    df_wbcorr <- cors_weighted@within$confidence_intervals

    print(df_statsby)
    print(df_wbcorr)


  }


  # run with pearson.
  cors_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal[2:5],
                          cluster = simdat_intensive_longitudinal$participantID,
                          weighted_between_statistics = FALSE))

  statsby <- suppressWarnings(psych::statsBy(simdat_intensive_longitudinal,
                                             group = 'participantID'))

  compare(cors_weighted, statsby)

  # run with spearman.
  cors_weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal[2:5],
                          cluster = simdat_intensive_longitudinal$participantID,
                          weighted_between_statistics = FALSE,
                          method = 'spearman'))

  statsby <- suppressWarnings(psych::statsBy(simdat_intensive_longitudinal,
                                             group = 'participantID',
                                             method = 'spearman'))

  compare(cors_weighted, statsby)
})

"
# on other data


try(dat <- readRDS("C:\\Users\\pascku\\OneDrive\\scripts\\01 R-Packages\\within-between-correlations\\test_data_real_factors.rds"))
#try(dat <- readRDS("C:\\Users\\kueng\\OneDrive\\scripts\\01 R-Packages\\within-between-correlations\\test_data_real.rds"))

exp_tables_pearson_real <- readRDS("testdata/exp_tables_pearson_real.rds")
exp_matrices_pearson_real <- readRDS("testdata/exp_matrices_pearson_real.rds")
exp_tables_spearman_real <- readRDS("testdata/exp_tables_spearman_real.rds")
exp_matrices_spearman_real <- readRDS("testdata/exp_matrices_spearman_real.rds")
exp_tables_jackknife_real <- readRDS("testdata/exp_tables_jackknife_real.rds")
exp_matrices_jackknife_real <- readRDS("testdata/exp_matrices_jackknife_real.rds")

test_that('all functions are correct on real output', {
  # Test pearson
  cors_not_weighted <- suppressWarnings(wbCorr(dat,
                          cluster = 'CoupleID',
                          weighted_between_statistics = FALSE))

  compare_to_expected_output(cors_not_weighted, exp_matrices_pearson_real, exp_tables_pearson_real)


  # Test spearman

  cors_not_weighted <- suppressWarnings(wbCorr(dat,
                              cluster = 'CoupleID',
                              method = 'spearman',
                              weighted_between_statistics = FALSE))
  compare_to_expected_output(cors_not_weighted, exp_matrices_spearman_real, exp_tables_spearman_real)
  # Test jackknife

  cors_not_weighted <- suppressWarnings(wbCorr(dat,
                              cluster = 'CoupleID',
                              method = 'spearman-jackknife'))
  compare_to_expected_output(cors_not_weighted, exp_matrices_jackknife_real, exp_tables_jackknife_real)


})

test_that('cluster specification is equivalent', {
  vers1 <- suppressWarnings(wbCorr(dat, dat$CoupleID, method = 'auto'))
  vers2 <- suppressWarnings(wbCorr(dat, 'CoupleID', method = 'auto'))

  expect_equal(get_table(vers1), get_tables(vers2))
  expect_equal(summary(vers1), get_matrix(vers2))
})

