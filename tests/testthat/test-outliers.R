library(testthat)
library(dataQI)

test_that("check_outliers returns correct structure", {
  df <- data.frame(x = c(1, 2, 3, 100), y = c(10, 20, 30, 40))
  result <- check_outliers(df)

  expect_type(result, "list")
  expect_named(result, c("column_stats", "outlier_rows", "outlier_score"))
})

test_that("check_outliers detects extreme outlier", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 100))
  result <- check_outliers(df)

  expect_equal(result$column_stats$outlier_count[1], 1L)
  expect_true("x" %in% names(result$outlier_rows))
  expect_equal(result$outlier_rows[["x"]], 6L)
})

test_that("check_outliers score is 1 when no outliers", {
  df <- data.frame(x = 1:10)
  result <- check_outliers(df)
  expect_equal(result$outlier_score, 1)
})

test_that("check_outliers returns NA score for non-numeric data", {
  df <- data.frame(a = letters[1:5])
  result <- check_outliers(df)
  expect_true(is.na(result$outlier_score))
  expect_equal(nrow(result$column_stats), 0L)
})

test_that("check_outliers respects multiplier", {
  df <- data.frame(x = c(rep(5, 10), 20))
  result_15 <- check_outliers(df, multiplier = 1.5)
  result_3  <- check_outliers(df, multiplier = 3)
  # multiplier=3 is more lenient, so it should detect fewer (or equal) outliers
  expect_true(result_3$column_stats$outlier_count <= result_15$column_stats$outlier_count)
})

test_that("check_outliers rejects invalid multiplier", {
  df <- data.frame(x = 1:5)
  expect_error(check_outliers(df, multiplier = -1), "'multiplier' must be a non-negative")
})

test_that("check_outliers rejects non-data-frame input", {
  expect_error(check_outliers(1:5), "'data' must be a data frame")
})
