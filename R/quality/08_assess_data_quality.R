# ============================================================================
# 08_assess_data_quality.R
# 데이터 자체 품질 검사 최종 실행 함수
# ============================================================================
# 사용 예시:
# source("R/suitability/00_variable_type.R")
# source("R/quality/01_quality_common.R")
# source("R/quality/02_check_missing.R")
# source("R/quality/03_check_duplicates.R")
# source("R/quality/04_check_outliers.R")
# source("R/quality/05_check_types.R")
# source("R/quality/06_check_format_consistency.R")
# source("R/quality/07_calculate_quality_score.R")
# source("R/quality/08_assess_data_quality.R")
#
# result <- assess_data_quality(data)
# result <- assess_data_quality(data, user_missing_codes = c(9, 99, 999))
# ============================================================================

assess_data_quality <- function(data, user_missing_codes = NULL) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  # 1. 결측값 표준화
  #    9, 99, 999는 user_missing_codes에 지정된 경우에만 결측 처리된다.
  data_clean <- quality_standardize_data(
    data = data,
    user_missing_codes = user_missing_codes
  )
  
  # 2. 변수 유형 자동 판정
  #    00_variable_type.R의 detect_all_variable_types() 기준을 사용한다.
  variable_types <- quality_detect_variable_types(data_clean)
  
  # 3. 항목별 품질 검사
  missing_result <- check_missing_quality(data_clean)
  type_result <- check_types_quality(data_clean, variable_types)
  format_result <- check_format_consistency_quality(data_clean)
  duplicate_result <- check_duplicates_quality(data_clean)
  outlier_result <- check_outliers_quality(data_clean, variable_types)
  structure_result <- check_variable_structure_quality(data_clean, variable_types)
  categorical_result <- check_categorical_quality(data_clean, variable_types)
  basic_result <- check_basic_structure_quality(data_clean, variable_types)
  
  # 4. 최종 점수 계산
  score_result <- calculate_quality_score(
    missing_result = missing_result,
    type_result = type_result,
    format_result = format_result,
    duplicate_result = duplicate_result,
    outlier_result = outlier_result,
    structure_result = structure_result,
    categorical_result = categorical_result,
    basic_result = basic_result
  )
  
  # 5. 추가 경고 문구 정리
  warnings <- character(0)
  
  if (!is.null(user_missing_codes) && length(user_missing_codes) > 0) {
    warnings <- c(
      warnings,
      paste0(
        "사용자 지정 결측 코드가 적용되었습니다: ",
        paste(user_missing_codes, collapse = ", ")
      )
    )
  }
  
  if (any(variable_types$variable_type == "날짜형 변수")) {
    warnings <- c(
      warnings,
      "날짜형 변수는 고유값이 많더라도 시간 정보를 나타내는 변수일 수 있으므로 ID성 변수 감점 대상에서 제외했습니다."
    )
  }
  
  if (!is.null(outlier_result$warning)) {
    warnings <- c(warnings, outlier_result$warning)
  }
  
  if (format_result$details$special_value_variable_count > 0) {
    warnings <- c(
      warnings,
      "일부 수치형 변수에 NaN, Inf, -Inf 값이 포함되어 있습니다. 이 값들은 평균 계산이나 그래프 생성 과정에서 오류를 만들 수 있으므로 분석 전에 처리해야 합니다."
    )
  }
  
  if (basic_result$details$empty_name_count > 0) {
    warnings <- c(
      warnings,
      "이름이 없는 변수가 포함되어 있습니다. 결과 해석과 코드 실행에 문제가 생길 수 있으므로 변수명을 수정하는 것이 좋습니다."
    )
  }
  
  if (basic_result$details$duplicated_name_count > 0) {
    warnings <- c(
      warnings,
      "동일한 변수명이 여러 개 존재합니다. 변수 구분이 어려워질 수 있으므로 중복 변수명을 수정하는 것이 좋습니다."
    )
  }
  
  if (basic_result$force_danger) {
    warnings <- c(
      warnings,
      "행, 열 또는 분석 가능한 변수가 부족하여 데이터 품질 진단이 어렵습니다. 파일이 올바르게 업로드되었는지 먼저 확인해야 합니다."
    )
  }
  
  # 6. 최종 결과 객체
  result <- list(
    total_score = score_result$total_score,
    final_status = score_result$final_status,
    final_message = score_result$final_message,
    common_result = score_result$common_result,
    item_scores = score_result$item_scores,
    major_issues = score_result$major_issues,
    warnings = unique(warnings),
    variable_types = variable_types,
    details = list(
      missing = missing_result,
      type = type_result,
      format = format_result,
      duplicate = duplicate_result,
      outlier = outlier_result,
      structure = structure_result,
      categorical = categorical_result,
      basic = basic_result
    ),
    data_clean = data_clean
  )
  
  return(result)
}
