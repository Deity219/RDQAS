library(testthat)
library(dataQI)

test_that("generate_report returns dataQI_report object", {
  df <- data.frame(
    age  = c(25, NA, 200, 30),
    name = c("Alice", "Bob", "Bob", "Dana")
  )
  report <- generate_report(df)
  expect_s3_class(report, "dataQI_report")
})

test_that("generate_report has required components", {
  df <- data.frame(x = c(1, NA, 3), y = c("a", "a", "c"))
  report <- generate_report(df)

  expect_named(report, c("completeness", "uniqueness", "validity",
                          "outliers", "consistency", "summary", "overall_score"))
})

test_that("generate_report overall_score is mean of component scores", {
  df <- data.frame(x = 1:10, y = letters[1:10])
  report <- generate_report(df)

  expected <- mean(c(
    report$completeness$completeness_score,
    report$uniqueness$uniqueness_score,
    report$validity$validity_score,
    report$outliers$outlier_score
  ), na.rm = TRUE)

  expect_equal(report$overall_score, expected)
})

test_that("generate_report includes consistency when rules provided", {
  df <- data.frame(a = 1:5, b = 6:10)
  rules <- list(a_lt_b = list(expr = "a < b"))
  report <- generate_report(df, consistency_rules = rules)

  expect_false(is.null(report$consistency))
  expect_equal(length(report$summary$dimension), 5)
})

test_that("print.dataQI_report runs without error", {
  df <- data.frame(x = c(1, NA, 3))
  report <- generate_report(df)
  expect_output(print(report), "Overall Quality Score")
})

test_that("generate_report rejects non-data-frame input", {
  expect_error(generate_report(1:5), "'data' must be a data frame")
})
