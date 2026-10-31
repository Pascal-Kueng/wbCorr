data("simdat_intensive_longitudinal")

test_that("cluster can be passed as a column name or vector", {
  by_name <- wbCorr(simdat_intensive_longitudinal, cluster = "participantID")
  by_vector <- wbCorr(simdat_intensive_longitudinal,
                      cluster = simdat_intensive_longitudinal$participantID)

  expect_equal(get_table(by_name), get_table(by_vector))
  expect_equal(get_ICC(by_name), get_ICC(by_vector))
  expect_false("participantID" %in% colnames(by_name@within$correlations))
  expect_false("participantID" %in% colnames(by_vector@within$correlations))
})

test_that("methods and inference options return the expected tables", {
  pearson <- wbCorr(simdat_intensive_longitudinal, "participantID")
  spearman <- wbCorr(simdat_intensive_longitudinal, "participantID",
                     method = "spearman")
  none <- wbCorr(simdat_intensive_longitudinal, "participantID",
                 inference = "none")

  expect_equal(colnames(pearson@within$table),
               c("Parameter1", "Parameter2", "pearson's r", "t-statistic",
                 "df", "95% CI", "p"))
  expect_equal(colnames(spearman@within$table),
               c("Parameter1", "Parameter2", "spearman's rho", "z-statistic",
                 "df", "95% CI", "p"))
  expect_equal(colnames(none@within$table),
               c("Parameter1", "Parameter2", "pearson's r"))
  expect_true(all(is.na(as.matrix(none@within$p_values))))
  expect_equal(none@within$correlations, pearson@within$correlations)
  expect_equal(summary(none, "w")$within["var1", "var2"], "0.12")
})

test_that("accessors return tables, matrices, and ICCs", {
  cors <- wbCorr(simdat_intensive_longitudinal, "participantID")

  expect_named(get_table(cors), c("within", "between"))
  expect_named(get_tables(cors, "b"), "between")
  expect_named(summary(cors), c("within", "between", "merged_wb", "note_wb",
                                "merged_bw", "note_bw", "note"))
  expect_named(get_matrix(cors, "wb"), c("merged_wb", "note"))
  expect_equal(base::summary(cors, "w"), get_matrix(cors, "w"))
  expect_equal(get_ICC(cors)$variable, c("day", "var1", "var2", "var3"))

  # day does not vary between participants
  between <- summary(cors, "b")$between
  expect_equal(between["day", "day"], "NA")
  expect_equal(between["var1", "var1"], "1")
})

test_that("print, plot, and to_excel work", {
  cors <- wbCorr(simdat_intensive_longitudinal, "participantID")

  expect_output(expect_invisible(print(cors)), "Within-Cluster Correlations")

  pdf(NULL)
  on.exit(dev.off())
  expect_message(graphics::plot(cors, "within"), "may take a while")
  expect_message(plot(cors, which = "b"), "may take a while")

  matrices_path <- to_excel(summary(cors), tempfile(fileext = ".xlsx"))
  table_path <- to_excel(get_table(cors)$within, tempfile(fileext = ".xlsx"))
  expect_true(file.exists(matrices_path))
  expect_true(file.exists(table_path))
})

test_that("cluster bootstrap resamples whole clusters", {
  dat <- simdat_intensive_longitudinal[c("participantID", "var1", "var2")]

  set.seed(1)
  boot <- wbCorr(dat, "participantID", inference = "cluster_bootstrap",
                 nboot = 200)
  expect_equal(colnames(boot@within$table),
               c("Parameter1", "Parameter2", "pearson's r", "BC 95% CI",
                 "bootstrap p"))

  # Within-cluster draws come first, so they can be reproduced by hand.
  set.seed(1)
  within <- boot@centered_data$within_df
  rows <- split(seq_len(nrow(dat)), dat$participantID)
  boot_r <- replicate(200, {
    idx <- unlist(rows[sample.int(length(rows), replace = TRUE)],
                  use.names = FALSE)
    cor(within$var1[idx], within$var2[idx])
  })
  r <- cor(within$var1, within$var2)
  z0 <- qnorm(mean(boot_r < r))
  expected <- quantile(boot_r, pnorm(2 * z0 + qnorm(c(0.025, 0.975))),
                       names = FALSE)

  ci <- boot@within$confidence_intervals
  expect_equal(c(ci$CI_lower, ci$CI_upper), expected)

  weighted <- wbCorr(dat, "participantID", inference = "cluster_bootstrap",
                     between_weighting = "cluster_size", nboot = 50)
  expect_false(anyNA(weighted@between$confidence_intervals$CI_lower))
})
