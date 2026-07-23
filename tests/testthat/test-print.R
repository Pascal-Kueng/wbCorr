test_that("print provides executable accessor guidance", {
  dat <- data.frame(
    id = rep(1:3, each = 3),
    x = 1:9,
    y = c(2, 1, 3, 5, 4, 6, 8, 7, 9)
  )
  result <- wbCorr(dat, cluster = "id", inference = "none")

  returned <- NULL
  output <- capture.output(returned <- print(result))
  output <- paste(output, collapse = "\n")

  expect_identical(returned, result)
  expect_match(
    output,
    "Inspect matrices with summary(object, which = c('w', 'b', 'wb'))",
    fixed = TRUE
  )
  expect_match(
    output,
    "Access matrices programmatically with get_matrix(object, numeric = TRUE)",
    fixed = TRUE
  )
  expect_match(
    output,
    "Access the full ICC table with get_ICC(object)",
    fixed = TRUE
  )
  expect_false(grepl("merge')", output, fixed = TRUE))
})
