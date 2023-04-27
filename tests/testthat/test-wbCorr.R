
data("simdat_intensive_longitudinal")
exp_tables <- readRDS("testdata/exp_tables.rds")
exp_matrices <- readRDS("testdata/exp_matrices.rds")

test_that("pearson stats are computed correctly", {
  simdat_intensive_longitudinal$participantID <- as.numeric(simdat_intensive_longitudinal$participantID)

  cors_weighted <- wbCorr(simdat_intensive_longitudinal,
                          cluster = 'participantID',
                          weighted_between_statistics = TRUE)

  tables <- get_table(cors_weighted)
  matrices <- summary(cors_weighted)

  expect_equal(tables, exp_tables)
  expect_equal(matrices, exp_matrices)
})
