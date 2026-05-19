# ============================================================================
# 02_check_missing.R
# 결측치 품질 검사
# ============================================================================
# 배점: 25점
# - 전체 결측률: 15점
# - 결측률 40% 이상 변수 비율: 10점
# ============================================================================

check_missing_quality <- function(data) {
  n_row <- nrow(data)
  n_col <- ncol(data)
  total_cells <- n_row * n_col
  
  total_missing_count <- if (total_cells == 0) 0 else sum(is.na(data))
  total_missing_rate <- quality_rate(total_missing_count, total_cells)
  
  if (n_col == 0) {
    variable_missing <- data.frame(
      variable = character(0),
      missing_count = integer(0),
      missing_rate = numeric(0),
      missing_percent = numeric(0),
      high_missing = logical(0),
      stringsAsFactors = FALSE
    )
  } else {
    missing_count <- vapply(seq_along(data), function(j) {
      sum(is.na(data[[j]]))
    }, integer(1))
    
    missing_rate <- if (n_row == 0) {
      rep(0, n_col)
    } else {
      missing_count / n_row
    }
    
    variable_missing <- data.frame(
      variable = names(data),
      missing_count = missing_count,
      missing_rate = round(missing_rate, 4),
      missing_percent = quality_percent(missing_rate),
      high_missing = missing_rate >= 0.40,
      stringsAsFactors = FALSE
    )
  }
  
  high_missing_variable_count <- sum(variable_missing$high_missing)
  high_missing_variable_rate <- quality_rate(high_missing_variable_count, n_col)
  
  total_missing_score <- score_missing_total(total_missing_rate)
  high_missing_score <- score_missing_high_variable(high_missing_variable_rate)
  score <- total_missing_score + high_missing_score
  
  return(list(
    item = "결측치 품질",
    score = score,
    max_score = 25,
    total_missing_rate = total_missing_rate,
    high_missing_variable_rate = high_missing_variable_rate,
    details = list(
      total_missing_count = total_missing_count,
      total_cells = total_cells,
      total_missing_percent = quality_percent(total_missing_rate),
      high_missing_variable_count = high_missing_variable_count,
      high_missing_variable_percent = quality_percent(high_missing_variable_rate),
      variable_missing = variable_missing,
      sub_scores = data.frame(
        criterion = c("전체 결측률", "결측률 40% 이상 변수 비율"),
        score = c(total_missing_score, high_missing_score),
        max_score = c(15, 10),
        stringsAsFactors = FALSE
      )
    )
  ))
}
