test_that("cluster can be passed as a column name or vector", {
  data("simdat_intensive_longitudinal")

  by_name <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                     cluster = "participantID",
                                     method = "auto"))
  by_vector <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                       cluster = simdat_intensive_longitudinal$participantID,
                                       method = "auto"))

  expect_equal(get_table(by_name), get_table(by_vector))
  expect_equal(get_matrix(by_name), get_matrix(by_vector))
  expect_false("participantID" %in% colnames(by_name@within$correlations))
  expect_false("participantID" %in% colnames(by_name@between$correlations))
})

test_that("main correlation methods return complete bundled-data output", {
  data("simdat_intensive_longitudinal")

  pearson <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                     cluster = "participantID"))
  spearman <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                      cluster = "participantID",
                                      method = "spearman"))
  jackknife <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                       cluster = "participantID",
                                       method = "spearman-jackknife"))

  expect_s4_class(pearson, "wbCorr")
  expect_s4_class(spearman, "wbCorr")
  expect_s4_class(jackknife, "wbCorr")

  expect_equal(dim(pearson@within$correlations), c(4L, 4L))
  expect_equal(dim(pearson@between$correlations), c(4L, 4L))
  expect_equal(nrow(pearson@within$table), 6L)
  expect_equal(nrow(pearson@between$table), 6L)

  expect_equal(nrow(spearman@within$table), 6L)
  expect_equal(nrow(spearman@between$table), 6L)
  expect_equal(nrow(jackknife@within$table), 6L)
  expect_equal(nrow(jackknife@between$table), 6L)
})

test_that("within Pearson df accounts for estimated cluster means", {
  data("simdat_intensive_longitudinal")

  cors <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                  cluster = "participantID"))

  expected_df <- nrow(simdat_intensive_longitudinal) -
    length(unique(simdat_intensive_longitudinal$participantID)) - 1

  expect_true(all(cors@within$table$df == expected_df))
})

test_that("between weighting option is explicit and backwards compatible", {
  data("simdat_intensive_longitudinal")

  equal <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                   cluster = "participantID"))
  weighted <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                      cluster = "participantID",
                                      between_weighting = "cluster_size"))
  alias <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                   cluster = "participantID",
                                   weighted_between_statistics = TRUE))

  expect_equal(equal@settings$between_weighting, "equal_clusters")
  expect_equal(equal@settings$centering_rows, "pairwise_complete")
  expect_equal(weighted@settings$between_weighting, "cluster_size")
  expect_equal(alias@settings$between_weighting, "cluster_size")
  expect_equal(weighted@between$correlations, alias@between$correlations)
  expect_match(weighted@between$table$warning[4],
               "weighted between inference approximate")
})

test_that("between inference can be omitted", {
  data("simdat_intensive_longitudinal")

  cors <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                  cluster = "participantID",
                                  between_weighting = "cluster_size",
                                  between_inference = "none"))

  expect_false(is.na(cors@between$correlations["var1", "var2"]))
  expect_true(is.na(cors@between$p_values["var1", "var2"]))
  expect_true(is.na(cors@between$confidence_intervals$CI_lower[4]))
})

test_that("accessors return tables and matrices", {
  data("simdat_intensive_longitudinal")

  cors <- suppressWarnings(wbCorr(simdat_intensive_longitudinal,
                                  cluster = "participantID"))

  tables <- get_table(cors)
  matrices <- get_matrix(cors)

  expect_named(tables, c("within", "between"))
  expect_true(all(c("within", "between", "merged_wb", "merged_bw") %in%
                    names(matrices)))
})
