#' Check cross-column consistency
#'
#' Evaluates user-supplied cross-column consistency rules and flags rows that
#' violate any rule.
#'
#' @param data A data frame to check.
#' @param rules A named list of rules.  Each rule is a named list with:
#'   \describe{
#'     \item{expr}{A character string containing an R expression (evaluated in
#'       the context of \code{data}) that returns a logical vector the same
#'       length as \code{nrow(data)}.  Rows where the expression is \code{TRUE}
#'       are considered \strong{valid}.}
#'     \item{description}{Optional human-readable description of the rule.}
#'   }
#' @return A list with:
#'   \describe{
#'     \item{rule_stats}{Data frame with per-rule pass/fail counts.}
#'     \item{violated_rows}{Named list of integer vectors giving the row
#'       indices that violated each rule.}
#'     \item{consistency_score}{Proportion of (row × rule) combinations that
#'       passed (0 to 1).}
#'   }
#' @examples
#' df <- data.frame(start = c(1, 5, 3), end = c(4, 2, 6))
#' rules <- list(
#'   start_before_end = list(
#'     expr = "start <= end",
#'     description = "start must not exceed end"
#'   )
#' )
#' result <- check_consistency(df, rules)
#' result$consistency_score
#' @export
check_consistency <- function(data, rules) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!is.list(rules) || length(rules) == 0) {
    stop("'rules' must be a non-empty named list.")
  }
  if (is.null(names(rules)) || any(names(rules) == "")) {
    stop("All elements of 'rules' must be named.")
  }

  n_rows <- nrow(data)

  rule_names   <- names(rules)
  pass_counts  <- integer(length(rules))
  fail_counts  <- integer(length(rules))
  descriptions <- character(length(rules))
  violated_rows <- list()

  for (i in seq_along(rules)) {
    rule  <- rules[[i]]
    rname <- rule_names[i]
    descriptions[i] <- if (!is.null(rule$description)) rule$description else ""

    result <- tryCatch(
      eval(parse(text = rule$expr), envir = data, enclos = parent.frame()),
      error = function(e) {
        warning(sprintf("Rule '%s' evaluation error: %s", rname, conditionMessage(e)))
        rep(NA, n_rows)
      }
    )

    if (!is.logical(result) || length(result) != n_rows) {
      warning(sprintf("Rule '%s' must evaluate to a logical vector of length %d; skipping.",
                      rname, n_rows))
      pass_counts[i] <- NA_integer_
      fail_counts[i] <- NA_integer_
      next
    }

    fails <- which(!result | is.na(result))
    fail_counts[i] <- length(fails)
    pass_counts[i] <- n_rows - length(fails)
    if (length(fails) > 0) {
      violated_rows[[rname]] <- fails
    }
  }

  rule_stats <- data.frame(
    rule        = rule_names,
    description = descriptions,
    pass_count  = pass_counts,
    fail_count  = fail_counts,
    pass_ratio  = round(pass_counts / n_rows, 4),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  total_checks  <- sum(!is.na(pass_counts)) * n_rows
  total_passes  <- sum(pass_counts, na.rm = TRUE)

  list(
    rule_stats        = rule_stats,
    violated_rows     = violated_rows,
    consistency_score = if (total_checks > 0) total_passes / total_checks else NA_real_
  )
}
