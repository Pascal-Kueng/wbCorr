test_that("Excel preparation accepts direct data frames and matrices", {
  prepare <- getFromNamespace("prepare_excel_sheets", "wbCorr")

  data_frame <- data.frame(
    estimate = c(0.123456789, NA_real_),
    label = c("first", "second"),
    row.names = c("pair_a", "pair_b"),
    check.names = FALSE
  )
  matrix_input <- matrix(
    c(1.25, 2.5, 3.75, 5),
    nrow = 2,
    dimnames = list(c("row_a", "row_b"), c("x", "y"))
  )

  data_frame_sheets <- prepare(data_frame)
  matrix_sheets <- prepare(matrix_input)
  expect_named(data_frame_sheets, "Sheet1")
  expect_named(matrix_sheets, "Sheet1")
  expect_identical(data_frame_sheets$Sheet1$RowName,
                   c("pair_a", "pair_b"))
  expect_equal(data_frame_sheets$Sheet1$estimate,
               data_frame$estimate,
               tolerance = 0)
  expect_identical(matrix_sheets$Sheet1$RowName,
                   c("row_a", "row_b"))
  expect_equal(as.matrix(matrix_sheets$Sheet1[, c("x", "y")]),
               matrix_input,
               tolerance = 0,
               ignore_attr = TRUE)
})


test_that("Excel preparation supports mixed lists and ignores notes", {
  prepare <- getFromNamespace("prepare_excel_sheets", "wbCorr")
  data_frame <- data.frame(x = 1:2)
  matrix_input <- matrix(3:6, nrow = 2)

  sheets <- prepare(list(table = data_frame,
                         matrix = matrix_input,
                         note = "display-only text"))
  expect_named(sheets, c("table", "matrix"))
  expect_true(all(vapply(sheets, is.data.frame, logical(1))))

  unnamed <- prepare(list(data_frame, matrix_input))
  expect_named(unnamed, c("Sheet1", "Sheet2"))

  duplicated <- prepare(list(result = data_frame, result = matrix_input))
  expect_named(duplicated, c("result", "result_1"))

  expect_error(prepare("not tabular"), "data frame, matrix")
  expect_error(prepare(list(note = "not tabular")),
               "at least one data frame or matrix")
})


test_that("to_excel writes direct and mixed tabular inputs", {
  data_frame <- data.frame(
    estimate = c(0.123456789, -0.987654321),
    row.names = c("a", "b")
  )
  matrix_input <- matrix(
    c(1, 0.25, 0.25, 1),
    nrow = 2,
    dimnames = list(c("x", "y"), c("x", "y"))
  )

  data_frame_path <- tempfile(fileext = ".xlsx")
  matrix_path <- tempfile(fileext = ".xlsx")
  mixed_path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(c(data_frame_path, matrix_path, mixed_path)), add = TRUE)

  data_frame_result <- withVisible(to_excel(data_frame, data_frame_path))
  matrix_result <- withVisible(to_excel(matrix_input, matrix_path))
  mixed_result <- withVisible(
    to_excel(list(estimates = data_frame,
                  correlations = matrix_input,
                  note = "ignored"),
             mixed_path)
  )

  expect_identical(data_frame_result$value, data_frame_path)
  expect_identical(matrix_result$value, matrix_path)
  expect_identical(mixed_result$value, mixed_path)
  expect_false(data_frame_result$visible)
  expect_false(matrix_result$visible)
  expect_false(mixed_result$visible)
  expect_true(all(file.exists(c(data_frame_path, matrix_path, mixed_path))))
  expect_true(all(file.info(c(data_frame_path, matrix_path, mixed_path))$size > 0))

  mixed_archive <- utils::unzip(mixed_path, list = TRUE)$Name
  worksheet_files <- grepl("^xl/worksheets/sheet[0-9]+[.]xml$",
                           mixed_archive)
  expect_equal(sum(worksheet_files), 2L)
})


test_that("to_excel rejects invalid paths and empty tabular input", {
  expect_error(to_excel(data.frame(x = 1), path = NA_character_), "path")
  expect_error(to_excel(data.frame(x = 1), path = character(0)), "path")
  expect_error(to_excel(data.frame(x = 1), path = c("a.xlsx", "b.xlsx")),
               "path")
  expect_error(to_excel(list(note = "nothing to write"),
                        tempfile(fileext = ".xlsx")),
               "at least one data frame or matrix")
})
