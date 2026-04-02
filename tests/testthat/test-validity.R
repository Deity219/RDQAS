library(testthat)
library(dataQI)

test_that("check_validity returns correct structure", {
  df <- data.frame(age = c(25, -1, 200, NA), name = c("Alice", "Bob", "", "Dana"))
  result <- check_validity(df)

  expect_type(result, "list")
  expect_named(result, c("column_stats", "invalid_rows", "validity_score"))
})

test_that("check_validity detects range violations", {
  df <- data.frame(age = c(25, -1, 200, 30))
  rules <- list(age = list(type = "numeric", min = 0, max = 150))
  result <- check_validity(df, rules)

  expect_equal(result$column_stats$invalid_count[result$column_stats$column == "age"], 2L)
})

test_that("check_validity detects allowed-value violations", {
  df <- data.frame(status = c("active", "inactive", "unknown"))
  rules <- list(status = list(allowed = c("active", "inactive")))
  result <- check_validity(df, rules)

  expect_equal(result$column_stats$invalid_count[result$column_stats$column == "status"], 1L)
  expect_equal(result$invalid_rows[["status"]], 3L)
})

test_that("check_validity detects pattern violations", {
  df <- data.frame(email = c("a@b.com", "notanemail", "c@d.org"))
  rules <- list(email = list(pattern = "^[^@]+@[^@]+\\.[^@]+$"))
  result <- check_validity(df, rules)

  expect_equal(result$column_stats$invalid_count[result$column_stats$column == "email"], 1L)
})

test_that("check_validity score is 1 with no rules and no violations", {
  df <- data.frame(x = 1:5)
  result <- check_validity(df)
  expect_equal(result$validity_score, 1)
})

test_that("check_validity rejects non-data-frame input", {
  expect_error(check_validity(1:5), "'data' must be a data frame")
})
