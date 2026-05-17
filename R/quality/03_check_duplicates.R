# ============================================================================
# 03_check_duplicates.R
# 중복값 품질 검사
# ============================================================================
# 배점: 15점
# - 완전히 동일한 행이 반복되는 경우 완전 중복 행으로 판단
# ============================================================================

check_duplicates_quality <- function(data) {
  n_row <- nrow(data)
  
  duplicate_count <- if (n_row <= 1) {
    0
  } else {
    sum(duplicated(data))
  }
  
  duplicate_rate <- quality_rate(duplicate_count, n_row)
  score <- score_duplicate(duplicate_rate)
  
  duplicate_rows <- if (n_row <= 1) {
    integer(0)
  } else {
    which(duplicated(data))
  }
  
  return(list(
    item = "중복값 품질",
    score = score,
    max_score = 15,
    duplicate_rate = duplicate_rate,
    details = list(
      duplicate_count = duplicate_count,
      total_rows = n_row,
      duplicate_percent = quality_percent(duplicate_rate),
      duplicate_rows = duplicate_rows
    )
  ))
}
