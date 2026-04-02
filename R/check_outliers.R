#' Check data for outliers
#'
#' Detects outliers in numeric columns using the IQR (interquartile range)
#' method.  A value is flagged as an outlier if it falls below
#' \code{Q1 - multiplier * IQR} or above \code{Q3 + multiplier * IQR}.
#'
#' @param data A data frame to check.
#' @param multiplier Numeric scalar controlling the whisker width.  Default is
#'   \code{1.5} (standard Tukey fences).
#' @return A list with:
#'   \describe{
#'     \item{column_stats}{Data frame with per-column outlier counts, bounds,
#'       and the proportion of non-NA values flagged as outliers.}
#'     \item{outlier_rows}{Named list of integer vectors giving the row indices
#'       that contain outliers for each numeric column.}
#'     \item{outlier_score}{Proportion of non-outlier numeric cells (0 to 1).
#'       Returns \code{NA} when there are no numeric columns.}
#'   }
#' @examples
#' df <- data.frame(x = c(1, 2, 3, 100), y = c(10, 20, 30, 40))
#' result <- check_outliers(df)
#' result$column_stats
#' @export
check_outliers <- function(data, multiplier = 1.5) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!is.numeric(multiplier) || length(multiplier) != 1 || multiplier < 0) {
    stop("'multiplier' must be a non-negative numeric scalar.")
  }

  numeric_cols <- names(data)[vapply(data, is.numeric, logical(1))]

  if (length(numeric_cols) == 0) {
    return(list(
      column_stats  = data.frame(
        column        = character(0),
        lower_bound   = numeric(0),
        upper_bound   = numeric(0),
        outlier_count = integer(0),
        outlier_ratio = numeric(0),
        stringsAsFactors = FALSE
      ),
      outlier_rows  = list(),
      outlier_score = NA_real_
    ))
  }

  results <- lapply(numeric_cols, function(col) {
    vals   <- data[[col]]
    non_na <- vals[!is.na(vals)]
    q1     <- stats::quantile(non_na, 0.25)
    q3     <- stats::quantile(non_na, 0.75)
    iqr    <- q3 - q1
    lower  <- q1 - multiplier * iqr
    upper  <- q3 + multiplier * iqr
    flags  <- !is.na(vals) & (vals < lower | vals > upper)
    list(
      lower         = lower,
      upper         = upper,
      outlier_count = sum(flags),
      outlier_ratio = if (length(non_na) > 0) round(sum(flags) / length(non_na), 4) else 0,
      row_indices   = which(flags)
    )
  })
  names(results) <- numeric_cols

  column_stats <- data.frame(
    column        = numeric_cols,
    lower_bound   = vapply(results, `[[`, numeric(1), "lower"),
    upper_bound   = vapply(results, `[[`, numeric(1), "upper"),
    outlier_count = vapply(results, `[[`, integer(1), "outlier_count"),
    outlier_ratio = vapply(results, `[[`, numeric(1), "outlier_ratio"),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  outlier_rows <- lapply(results, `[[`, "row_indices")
  outlier_rows <- outlier_rows[vapply(outlier_rows, length, integer(1)) > 0]

  total_numeric_vals  <- sum(vapply(data[, numeric_cols, drop = FALSE],
                                    function(x) sum(!is.na(x)), integer(1)))
  total_outlier_vals  <- sum(column_stats$outlier_count)

  list(
    column_stats  = column_stats,
    outlier_rows  = outlier_rows,
    outlier_score = if (total_numeric_vals > 0)
      1 - total_outlier_vals / total_numeric_vals else NA_real_
  )
}
