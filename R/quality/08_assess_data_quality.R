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
# [파일 역할]
# - R/quality 폴더에 나누어 둔 모든 품질 검사 함수를 실제로 실행하는 최종 함수 파일이다.
# - 최종 사용 함수는 assess_data_quality(data)이다.
#
# [수정항목 반영 내역]
# - 수정항목 1) 품질 점수 계산 기준 보완
#   * 02_check_missing.R의 보완된 결측치 검사 결과를 받아 최종 점수에 반영한다.
#
# - 수정항목 2) 범주형 데이터의 중복 행 판정 완화
#   * 03_check_duplicates.R에 variable_types를 함께 넘겨 범주형 반복 데이터인지 판단할 수 있게 했다.
#
# - 수정항목 3) 컬럼별 중복률 개념 정리
#   * result$column_summary에 컬럼별 고유값 수와 고유값 비율을 제공한다.
#   * 컬럼별 "중복률"이라는 표현은 사용하지 않는다.
#
# - 수정항목 4) CSV 인코딩 및 한글 변수명 처리 보완
#   * 실제 업로드 인코딩 선택은 03_ui.R/04_server.R에서 연결할 예정이다.
#   * 이 파일에서는 check.names = FALSE를 사용해 한글 변수명과 원래 변수명을 최대한 보존한다.
#
# - 수정항목 5) 빈 데이터/전체 결측 변수/상수 변수/중복 변수명 예외 처리
#   * quality_standardize_data(), quality_detect_variable_types(), check_basic_structure_quality()를 통해
#     앱이 멈추지 않고 안내 문구를 반환하도록 구성했다.
#
# - 수정항목 6) XLSX 업로드 경로 점검
#   * 실제 XLSX 읽기는 01_quality_common.R의 quality_read_xlsx()와 나중에 수정할 server에서 처리한다.
#   * assess_data_quality()는 CSV에서 읽은 data.frame이든 XLSX에서 읽은 data.frame이든 동일하게 검사한다.
#
# - 수정항목 7) quality_report 반환 구조 정리
#   * 최종 반환 객체에 common_result, item_scores, major_issues, warnings, details를 포함한다.
#   * 기존 04_server.R과 임시 호환되도록 score, missing, outliers, format 필드도 함께 둔다.
# ============================================================================

assess_data_quality <- function(data, user_missing_codes = NULL) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  }
  
  # 1. 결측값 표준화
  # 수정항목 5:
  # - "NA", "N/A", "NULL", 빈 문자열 등을 실제 NA로 통일한다.
  # - 사용자가 지정한 결측 코드가 있을 때만 추가 결측 처리한다.
  data_clean <- quality_standardize_data(
    data = data,
    user_missing_codes = user_missing_codes
  )
  
  # 2. 변수 유형 자동 판정
  # 수정항목 5:
  # - 00_variable_type.R의 detect_all_variable_types()를 우선 사용하되,
  #   오류가 나면 01_quality_common.R의 fallback이 작동한다.
  variable_types <- quality_detect_variable_types(data_clean)
  
  # 3. 컬럼별 요약표 생성
  # 수정항목 3:
  # - 컬럼별 중복률 대신 고유값 수와 고유값 비율을 제공한다.
  column_summary <- quality_create_column_summary(data_clean, variable_types)
  
  # 4. 항목별 품질 검사
  missing_result <- check_missing_quality(data_clean)
  type_result <- check_types_quality(data_clean, variable_types)
  format_result <- check_format_consistency_quality(data_clean)
  
  # 수정항목 2:
  # - 중복 행 검사에 variable_types를 함께 넘겨 범주형 반복 데이터 완화 여부를 판단한다.
  duplicate_result <- check_duplicates_quality(data_clean, variable_types)
  
  outlier_result <- check_outliers_quality(data_clean, variable_types)
  structure_result <- check_variable_structure_quality(data_clean, variable_types)
  categorical_result <- check_categorical_quality(data_clean, variable_types)
  basic_result <- check_basic_structure_quality(data_clean, variable_types)
  
  # 5. 최종 점수 계산
  # 수정항목 7:
  # - common_result, item_scores, major_issues를 한 번에 만든다.
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
  
  # 6. 추가 경고 문구 정리
  # 수정항목 2, 4, 5, 6과 관련된 안내 문구를 한곳에 모은다.
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
  
  if (isTRUE(duplicate_result$relaxed)) {
    warnings <- c(warnings, duplicate_result$message)
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
  
  if (categorical_result$skipped) {
    warnings <- c(warnings, categorical_result$warning)
  }
  
  if (basic_result$details$name_problem_count > 0) {
    warnings <- c(
      warnings,
      "변수명이 비어 있거나 중복된 변수가 포함되어 있습니다. 결과 해석과 코드 실행에 문제가 생길 수 있으므로 변수명을 수정하는 것이 좋습니다."
    )
  }
  
  if (basic_result$force_danger) {
    warnings <- c(
      warnings,
      "행, 열 또는 분석 가능한 변수가 부족하여 데이터 품질 진단이 어렵습니다. 파일이 올바르게 업로드되었는지 먼저 확인해야 합니다."
    )
  }
  
  warnings <- unique(warnings)
  
  # 기존 04_server.R 임시 호환용 format 필드 구성
  # 자료형 오류 의심 변수와 특수값 포함 변수를 하나로 묶어 둔다.
  legacy_format <- type_result$details$type_issue_table
  
  special_problem_table <- format_result$details$special_table[
    format_result$details$special_table$has_special_value,
    ,
    drop = FALSE
  ]
  
  if (nrow(special_problem_table) > 0) {
    special_legacy <- data.frame(
      variable = special_problem_table$variable,
      issue = "NaN, Inf 또는 -Inf 포함",
      stringsAsFactors = FALSE
    )
    legacy_format <- rbind(
      legacy_format[, c("variable", "issue"), drop = FALSE],
      special_legacy
    )
  }
  
  # 7. 최종 결과 반환
  # 수정항목 7:
  # - 보고서 담당자가 사용할 수 있는 구조와 기존 server 임시 호환 필드를 함께 제공한다.
  result <- list(
    total_score = score_result$total_score,
    score = score_result$total_score,  # 기존 04_server.R 호환용 별칭
    
    final_status = score_result$final_status,
    final_message = score_result$final_message,
    
    common_result = score_result$common_result,
    item_scores = score_result$item_scores,
    major_issues = score_result$major_issues,
    warnings = warnings,
    
    variable_types = variable_types,
    column_summary = column_summary,
    data_clean = data_clean,
    
    details = list(
      missing = missing_result,
      type = type_result,
      format = format_result,
      duplicate = duplicate_result,
      outlier = outlier_result,
      variable_structure = structure_result,
      categorical = categorical_result,
      basic_structure = basic_result,
      column_summary = column_summary
    ),
    
    # 기존 04_server.R 임시 호환용 필드
    missing = missing_result$details$variable_missing,
    outliers = outlier_result$details$outlier_table,
    format = legacy_format
  )
  
  return(result)
}
