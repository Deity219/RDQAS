library(testthat)
library(dataQI)

test_that("check_completeness returns correct structure", {
  df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
  result <- check_completeness(df)

  expect_type(result, "list")
  expect_named(result, c("column_stats", "total_missing", "total_cells", "completeness_score"))
  expect_s3_class(result$column_stats, "data.frame")
  expect_equal(nrow(result$column_stats), 2L)
})

test_that("check_completeness calculates missing counts correctly", {
  df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
  result <- check_completeness(df)

  stats <- result$column_stats
  expect_equal(stats$missing_count[stats$column == "a"], 1L)
  expect_equal(stats$missing_count[stats$column == "b"], 1L)
  expect_equal(result$total_missing, 2L)
  expect_equal(result$total_cells, 6L)
})

test_that("check_completeness score is 1 for complete data", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  result <- check_completeness(df)
  expect_equal(result$completeness_score, 1)
})

test_that("check_completeness score is 0 for fully missing data", {
  df <- data.frame(x = NA_real_, y = NA_character_)
  result <- check_completeness(df)
  expect_equal(result$completeness_score, 0)
})

test_that("check_completeness handles empty data frame", {
  df <- data.frame(a = integer(0), b = character(0))
  result <- check_completeness(df)
  expect_equal(result$total_missing, 0L)
  expect_true(is.na(result$completeness_score))
})

test_that("check_completeness rejects non-data-frame input", {
  expect_error(check_completeness(matrix(1:4, 2)), "'data' must be a data frame")
})
