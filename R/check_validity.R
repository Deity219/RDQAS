#' Check data validity
#'
#' Validates data types and optional value ranges / allowed values for each
#' column.  Returns per-column invalid-value counts and an overall validity
#' score.
#'
#' @param data A data frame to check.
#' @param rules A named list of validation rules.  Each element should be a
#'   list with any combination of:
#'   \describe{
#'     \item{type}{Expected R class string, e.g. \code{"numeric"},
#'       \code{"character"}, \code{"Date"}.}
#'     \item{min}{Minimum allowed value (numeric columns).}
#'     \item{max}{Maximum allowed value (numeric columns).}
#'     \item{allowed}{Character vector of allowed values.}
#'     \item{pattern}{Regular expression that values must match (character
#'       columns).}
#'   }
#'   If \code{NULL} (default), only basic type information is reported.
#' @return A list with:
#'   \describe{
#'     \item{column_stats}{Data frame with per-column type, invalid count and
#'       validity ratio.}
#'     \item{invalid_rows}{Named list of integer vectors giving the row indices
#'       that failed validation for each rule-checked column.}
#'     \item{validity_score}{Overall proportion of valid cells (0 to 1).}
#'   }
#' @examples
#' df <- data.frame(age = c(25, -1, 200, NA), name = c("Alice", "Bob", "", "Dana"))
#' rules <- list(age = list(type = "numeric", min = 0, max = 150))
#' result <- check_validity(df, rules)
#' result$validity_score
#' @export
check_validity <- function(data, rules = NULL) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }

  n_rows <- nrow(data)
  col_names <- names(data)

  col_types <- vapply(data, function(col) paste(class(col), collapse = "/"), character(1))

  invalid_counts <- rep(0L, ncol(data))
  names(invalid_counts) <- col_names
  invalid_rows <- list()

  if (!is.null(rules)) {
    for (col in intersect(names(rules), col_names)) {
      rule  <- rules[[col]]
      vals  <- data[[col]]
      bad   <- rep(FALSE, n_rows)

      if (!is.null(rule$type)) {
        expected <- rule$type
        if (!inherits(vals, expected)) {
          converted <- tryCatch(
            switch(expected,
              numeric   = suppressWarnings(as.numeric(as.character(vals))),
              integer   = suppressWarnings(as.integer(as.character(vals))),
              character = as.character(vals),
              logical   = suppressWarnings(as.logical(vals)),
              vals
            ),
            error = function(e) vals
          )
          bad <- bad | (is.na(converted) & !is.na(vals))
          vals <- converted
        }
      }

      if (!is.null(rule$min) && is.numeric(vals)) {
        bad <- bad | (!is.na(vals) & vals < rule$min)
      }
      if (!is.null(rule$max) && is.numeric(vals)) {
        bad <- bad | (!is.na(vals) & vals > rule$max)
      }
      if (!is.null(rule$allowed)) {
        bad <- bad | (!is.na(vals) & !(as.character(vals) %in% as.character(rule$allowed)))
      }
      if (!is.null(rule$pattern) && is.character(vals)) {
        bad <- bad | (!is.na(vals) & !grepl(rule$pattern, vals, perl = TRUE))
      }

      invalid_counts[col] <- sum(bad)
      if (any(bad)) {
        invalid_rows[[col]] <- which(bad)
      }
    }
  }

  column_stats <- data.frame(
    column        = col_names,
    type          = col_types,
    invalid_count = invalid_counts,
    validity_ratio = round(1 - invalid_counts / pmax(n_rows, 1L), 4),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  total_cells   <- n_rows * ncol(data)
  total_invalid <- sum(invalid_counts)

  list(
    column_stats   = column_stats,
    invalid_rows   = invalid_rows,
    validity_score = if (total_cells > 0) 1 - total_invalid / total_cells else NA_real_
  )
}
