#' Check data uniqueness (duplicate detection)
#'
#' Identifies duplicate rows in a data frame and returns uniqueness statistics,
#' including per-column cardinality and overall uniqueness scores.
#'
#' @param data A data frame to check.
#' @param key_cols Character vector of column names to use as the key for
#'   duplicate detection. If \code{NULL} (default), all columns are used.
#' @return A list with:
#'   \describe{
#'     \item{duplicate_rows}{Data frame of duplicate rows (excluding the first
#'       occurrence).}
#'     \item{duplicate_count}{Number of duplicate rows found.}
#'     \item{uniqueness_score}{Proportion of unique rows (0 to 1).}
#'     \item{column_stats}{Data frame with per-column unique-value counts and
#'       ratios.}
#'   }
#' @examples
#' df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
#' result <- check_uniqueness(df)
#' result$duplicate_count
#' @export
check_uniqueness <- function(data, key_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!is.null(key_cols)) {
    bad <- setdiff(key_cols, names(data))
    if (length(bad) > 0) {
      stop("Columns not found in data: ", paste(bad, collapse = ", "))
    }
    subset_data <- data[, key_cols, drop = FALSE]
  } else {
    subset_data <- data
  }

  n_rows <- nrow(data)

  if (n_rows == 0) {
    return(list(
      duplicate_rows    = data[integer(0), , drop = FALSE],
      duplicate_count   = 0L,
      uniqueness_score  = NA_real_,
      column_stats      = data.frame(
        column       = names(data),
        unique_count = rep(0L, ncol(data)),
        unique_ratio = rep(0, ncol(data)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    ))
  }

  is_dup         <- duplicated(subset_data)
  duplicate_rows <- data[is_dup, , drop = FALSE]
  duplicate_count <- sum(is_dup)

  unique_counts <- vapply(data, function(col) length(unique(col[!is.na(col)])), integer(1))
  unique_ratios <- round(unique_counts / n_rows, 4)

  column_stats <- data.frame(
    column       = names(data),
    unique_count = unique_counts,
    unique_ratio = unique_ratios,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  list(
    duplicate_rows   = duplicate_rows,
    duplicate_count  = duplicate_count,
    uniqueness_score = 1 - duplicate_count / n_rows,
    column_stats     = column_stats
  )
}
