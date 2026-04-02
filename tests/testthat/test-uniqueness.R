library(testthat)
library(dataQI)

test_that("check_uniqueness returns correct structure", {
  df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  result <- check_uniqueness(df)

  expect_type(result, "list")
  expect_named(result, c("duplicate_rows", "duplicate_count", "uniqueness_score", "column_stats"))
})

test_that("check_uniqueness detects duplicate rows", {
  df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
  result <- check_uniqueness(df)

  expect_equal(result$duplicate_count, 1L)
  expect_equal(nrow(result$duplicate_rows), 1L)
})

test_that("check_uniqueness score is 1 for fully unique data", {
  df <- data.frame(a = 1:5, b = letters[1:5])
  result <- check_uniqueness(df)
  expect_equal(result$uniqueness_score, 1)
  expect_equal(result$duplicate_count, 0L)
})

test_that("check_uniqueness works with key_cols", {
  df <- data.frame(id = c(1, 1, 2), val = c("a", "b", "c"))
  result <- check_uniqueness(df, key_cols = "id")
  expect_equal(result$duplicate_count, 1L)
})

test_that("check_uniqueness errors on missing key columns", {
  df <- data.frame(a = 1:3)
  expect_error(check_uniqueness(df, key_cols = "z"), "Columns not found")
})

test_that("check_uniqueness handles empty data frame", {
  df <- data.frame(a = integer(0))
  result <- check_uniqueness(df)
  expect_equal(result$duplicate_count, 0L)
  expect_true(is.na(result$uniqueness_score))
})

test_that("check_uniqueness rejects non-data-frame input", {
  expect_error(check_uniqueness(list(a = 1:3)), "'data' must be a data frame")
})
