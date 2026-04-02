library(testthat)
library(dataQI)

test_that("check_consistency returns correct structure", {
  df <- data.frame(start = c(1, 5, 3), end = c(4, 2, 6))
  rules <- list(
    start_before_end = list(expr = "start <= end", description = "start <= end")
  )
  result <- check_consistency(df, rules)

  expect_type(result, "list")
  expect_named(result, c("rule_stats", "violated_rows", "consistency_score"))
})

test_that("check_consistency flags correct violated rows", {
  df <- data.frame(start = c(1, 5, 3), end = c(4, 2, 6))
  rules <- list(start_before_end = list(expr = "start <= end"))
  result <- check_consistency(df, rules)

  expect_equal(result$violated_rows[["start_before_end"]], 2L)
  expect_equal(result$rule_stats$fail_count[1], 1L)
  expect_equal(result$rule_stats$pass_count[1], 2L)
})

test_that("check_consistency score is 1 when all rules pass", {
  df <- data.frame(a = 1:5, b = 6:10)
  rules <- list(a_lt_b = list(expr = "a < b"))
  result <- check_consistency(df, rules)
  expect_equal(result$consistency_score, 1)
})

test_that("check_consistency errors on non-data-frame input", {
  expect_error(check_consistency(list(), list(r = list(expr = "TRUE"))),
               "'data' must be a data frame")
})

test_that("check_consistency errors on empty rules", {
  df <- data.frame(x = 1:3)
  expect_error(check_consistency(df, list()), "'rules' must be a non-empty named list")
})

test_that("check_consistency errors on unnamed rules", {
  df <- data.frame(x = 1:3)
  expect_error(check_consistency(df, list(list(expr = "x > 0"))),
               "All elements of 'rules' must be named")
})
