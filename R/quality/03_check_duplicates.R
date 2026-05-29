# ============================================================================
# 03_check_duplicates.R
# 중복값 품질 검사
# ============================================================================
# [파일 역할]
# - 완전히 동일한 행이 반복되는지 확인한다.
# - 행 단위 중복만 다루며, 컬럼 내부 반복값은 중복률로 표현하지 않는다.
# - 최종 배점은 15점이다.
#
# [수정항목 반영 내역]
# - 수정항목 2) 범주형 데이터의 중복 행 판정 완화
#   * Titanic처럼 범주형 변수 조합만으로 이루어진 데이터에서는 동일한 행 조합이
#     자연스럽게 반복될 수 있다.
#   * 따라서 수치형 변수가 없고 범주형/수치형 범주형 변수 중심인 데이터에서
#     중복 행이 발견되면 무조건 심각한 오류로 단정하지 않고,
#     "동일 조합 반복 데이터일 수 있음"이라는 안내 문구를 반환한다.
#   * 이 경우 중복 점수가 과도하게 낮아지지 않도록 최소 12점으로 완화한다.
#
# - 수정항목 3) 컬럼별 중복률 개념 정리
#   * 이 파일은 행 단위 중복만 담당한다.
#   * 변수 내부의 반복값은 중복률이라고 표현하지 않고,
#     01_quality_common.R의 quality_create_column_summary()에서 고유값 수/고유값 비율로 제공한다.
#
# - 수정항목 5) 예외 처리
#   * 행 수 0개 또는 1개인 데이터에서는 duplicated() 계산이 문제를 일으키지 않도록 처리한다.
# ============================================================================

check_duplicates_quality <- function(data, variable_types = NULL) {
  n_row <- nrow(data)
  
  duplicate_count <- if (n_row <= 1) {
    0
  } else {
    sum(duplicated(data))
  }
  
  duplicate_rate <- quality_rate(duplicate_count, n_row)
  raw_score <- score_duplicate(duplicate_rate)
  
  duplicate_rows <- if (n_row <= 1) {
    integer(0)
  } else {
    which(duplicated(data))
  }
  
  # 수정항목 2 반영:
  # 범주형 변수만 있는 데이터에서는 동일한 조합이 반복되는 것이 자연스러울 수 있다.
  # 예: Titanic 원자료처럼 Class/Sex/Age/Survived 조합이 여러 번 반복되는 경우
  categorical_repetition_possible <- FALSE
  
  if (!is.null(variable_types) && nrow(variable_types) > 0) {
    categorical_types <- c("범주형 변수", "수치형 범주 변수", "숫자형 범주 변수")
    numeric_count <- sum(variable_types$variable_type == "수치형 변수")
    categorical_count <- sum(variable_types$variable_type %in% categorical_types)
    
    categorical_repetition_possible <- duplicate_count > 0 &&
      numeric_count == 0 &&
      categorical_count > 0
  }
  
  if (categorical_repetition_possible) {
    score <- max(raw_score, 12)
    duplicate_message <- "동일한 행 조합이 반복되어 있습니다. 다만 수치형 변수 없이 범주형 변수 중심으로 구성된 데이터에서는 동일 조합 반복이 자연스러울 수 있습니다. 실제 중복 오류인지 원자료 구조상 자연스러운 반복인지 확인하세요."
  } else {
    score <- raw_score
    duplicate_message <- if (duplicate_count > 0) {
      "완전히 동일한 행이 반복되어 있습니다. 실제 중복 오류인지 확인하세요."
    } else {
      "완전 중복 행이 발견되지 않았습니다."
    }
  }
  
  return(list(
    item = "중복값 품질",
    score = score,
    max_score = 15,
    duplicate_rate = duplicate_rate,
    relaxed = categorical_repetition_possible,
    message = duplicate_message,
    details = list(
      duplicate_count = duplicate_count,
      total_rows = n_row,
      duplicate_percent = quality_percent(duplicate_rate),
      duplicate_rows = duplicate_rows,
      raw_score = raw_score,
      adjusted_score = score,
      categorical_repetition_possible = categorical_repetition_possible,
      note = duplicate_message
    )
  ))
}
