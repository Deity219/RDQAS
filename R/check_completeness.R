#' Check data completeness (missing values)
#'
#' Analyzes missing values in each column of a data frame and returns
#' completeness statistics including counts, percentages, and an overall
#' completeness score.
#'
#' @param data A data frame to check.
#' @return A list with:
#'   \describe{
#'     \item{column_stats}{Data frame with per-column missing value statistics.}
#'     \item{total_missing}{Total number of missing values across all columns.}
#'     \item{total_cells}{Total number of cells in the data frame.}
#'     \item{completeness_score}{Overall completeness ratio (0 to 1).}
#'   }
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
#' result <- check_completeness(df)
#' result$completeness_score
#' @export
check_completeness <- function(data) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (nrow(data) == 0) {
    return(list(
      column_stats   = data.frame(
        column          = character(0),
        missing_count   = integer(0),
        missing_pct     = numeric(0),
        present_count   = integer(0),
        present_pct     = numeric(0),
        stringsAsFactors = FALSE
      ),
      total_missing      = 0L,
      total_cells        = 0L,
      completeness_score = NA_real_
    ))
  }

  n_rows <- nrow(data)
  col_names <- names(data)

  missing_counts <- vapply(data, function(col) sum(is.na(col)), integer(1))
  missing_pct    <- missing_counts / n_rows * 100
  present_counts <- n_rows - missing_counts
  present_pct    <- present_counts / n_rows * 100

  column_stats <- data.frame(
    column        = col_names,
    missing_count = missing_counts,
    missing_pct   = round(missing_pct, 2),
    present_count = present_counts,
    present_pct   = round(present_pct, 2),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  total_cells   <- n_rows * ncol(data)
  total_missing <- sum(missing_counts)

  list(
    column_stats       = column_stats,
    total_missing      = total_missing,
    total_cells        = total_cells,
    completeness_score = if (total_cells > 0) 1 - total_missing / total_cells else NA_real_
  )
}
