# ============================================================================
# 06_check_format_consistency.R
# 특수값 NaN / Inf / -Inf 포함 여부 검사
# ============================================================================
# [파일 역할]
# - 수치형 변수에 NaN, Inf, -Inf가 포함되어 있는지 검사한다.
# - 자료형·형식 및 특수값 일관성 20점 중 "특수값 포함 변수 비율" 5점을 담당한다.
#
# [수정항목 반영 내역]
# - 수정항목 5) 예외 처리
#   * 수치형 변수가 없는 데이터에서도 오류가 나지 않고 5점으로 처리한다.
#   * NaN/Inf/-Inf는 평균 계산이나 그래프 생성 시 오류를 만들 수 있으므로
#     결측치나 이상치와 분리해서 별도 점검한다.
# ============================================================================

check_format_consistency_quality <- function(data) {
  numeric_idx <- which(vapply(data, function(x) is.numeric(x) || is.integer(x), logical(1)))
  
  if (length(numeric_idx) == 0) {
    special_table <- data.frame(
      variable = character(0),
      nan_count = integer(0),
      inf_count = integer(0),
      negative_inf_count = integer(0),
      has_special_value = logical(0),
      stringsAsFactors = FALSE
    )
    
    return(list(
      item = "NaN/Inf/-Inf 포함 여부",
      score = 5,
      max_score = 5,
      special_value_rate = 0,
      details = list(
        numeric_variable_count = 0,
        special_value_variable_count = 0,
        special_value_percent = 0,
        special_table = special_table
      )
    ))
  }
  
  special_table <- do.call(rbind, lapply(numeric_idx, function(j) {
    x <- data[[j]]
    
    nan_count <- sum(is.nan(x))
    inf_count <- sum(is.infinite(x) & x > 0, na.rm = TRUE)
    negative_inf_count <- sum(is.infinite(x) & x < 0, na.rm = TRUE)
    has_special_value <- (nan_count + inf_count + negative_inf_count) > 0
    
    data.frame(
      variable = names(data)[j],
      nan_count = nan_count,
      inf_count = inf_count,
      negative_inf_count = negative_inf_count,
      has_special_value = has_special_value,
      stringsAsFactors = FALSE
    )
  }))
  
  special_value_variable_count <- sum(special_table$has_special_value)
  special_value_rate <- quality_rate(special_value_variable_count, length(numeric_idx))
  score <- score_special_value(special_value_rate)
  
  return(list(
    item = "NaN/Inf/-Inf 포함 여부",
    score = score,
    max_score = 5,
    special_value_rate = special_value_rate,
    details = list(
      numeric_variable_count = length(numeric_idx),
      special_value_variable_count = special_value_variable_count,
      special_value_percent = quality_percent(special_value_rate),
      special_table = special_table
    )
  ))
}
