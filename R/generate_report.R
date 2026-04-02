#' Generate a comprehensive data quality report
#'
#' Runs all four quality checks (completeness, uniqueness, validity, outliers)
#' and optionally consistency checks, then aggregates the results into a single
#' report object with an overall quality score.
#'
#' @param data A data frame to assess.
#' @param validity_rules Optional named list of validity rules passed to
#'   \code{\link{check_validity}}.
#' @param consistency_rules Optional named list of consistency rules passed to
#'   \code{\link{check_consistency}}.
#' @param outlier_multiplier Numeric scalar passed to
#'   \code{\link{check_outliers}}. Default is \code{1.5}.
#' @return A list of class \code{"dataQI_report"} with components:
#'   \describe{
#'     \item{completeness}{Result of \code{check_completeness}.}
#'     \item{uniqueness}{Result of \code{check_uniqueness}.}
#'     \item{validity}{Result of \code{check_validity}.}
#'     \item{outliers}{Result of \code{check_outliers}.}
#'     \item{consistency}{Result of \code{check_consistency}, or \code{NULL}
#'       if no rules supplied.}
#'     \item{summary}{Data frame summarising each dimension's score.}
#'     \item{overall_score}{Numeric: mean of the available dimension scores
#'       (0 to 1).}
#'   }
#' @examples
#' df <- data.frame(
#'   age  = c(25, NA, 200, 30),
#'   name = c("Alice", "Bob", "Bob", "Dana")
#' )
#' report <- generate_report(df)
#' report$overall_score
#' @export
generate_report <- function(data,
                            validity_rules      = NULL,
                            consistency_rules   = NULL,
                            outlier_multiplier  = 1.5) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  completeness <- check_completeness(data)
  uniqueness   <- check_uniqueness(data)
  validity     <- check_validity(data, rules = validity_rules)
  outliers     <- check_outliers(data, multiplier = outlier_multiplier)
  consistency  <- if (!is.null(consistency_rules)) {
    check_consistency(data, consistency_rules)
  } else {
    NULL
  }

  scores <- c(
    completeness = completeness$completeness_score,
    uniqueness   = uniqueness$uniqueness_score,
    validity     = validity$validity_score,
    outliers     = outliers$outlier_score
  )
  if (!is.null(consistency)) {
    scores["consistency"] <- consistency$consistency_score
  }

  valid_scores <- scores[!is.na(scores)]
  overall      <- if (length(valid_scores) > 0) mean(valid_scores) else NA_real_

  summary_df <- data.frame(
    dimension = names(scores),
    score     = round(scores, 4),
    grade     = vapply(scores, .score_to_grade, character(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  report <- list(
    completeness  = completeness,
    uniqueness    = uniqueness,
    validity      = validity,
    outliers      = outliers,
    consistency   = consistency,
    summary       = summary_df,
    overall_score = overall
  )
  class(report) <- "dataQI_report"
  report
}

#' @export
print.dataQI_report <- function(x, ...) {
  cat("=== dataQI Data Quality Report ===\n\n")
  cat(sprintf("Overall Quality Score : %.1f%%  [%s]\n\n",
              x$overall_score * 100,
              .score_to_grade(x$overall_score)))
  cat("Dimension Scores:\n")
  for (i in seq_len(nrow(x$summary))) {
    row <- x$summary[i, ]
    cat(sprintf("  %-15s : %.1f%%  [%s]\n",
                row$dimension, row$score * 100, row$grade))
  }
  cat("\n")
  invisible(x)
}

# Internal helper: convert a 0-1 score to a letter grade.
.score_to_grade <- function(score) {
  if (is.na(score)) return("N/A")
  if (score >= 0.95) return("A")
  if (score >= 0.85) return("B")
  if (score >= 0.70) return("C")
  if (score >= 0.50) return("D")
  return("F")
}
