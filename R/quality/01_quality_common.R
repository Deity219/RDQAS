# ============================================================================
# 01_quality_common.R
# RDQAS 데이터 품질 검사 공통 함수
# ============================================================================
# 사용 전제:
# - R/suitability/00_variable_type.R을 먼저 source 해야 함
# - 여기서는 00_variable_type.R의 normalize_missing(), detect_all_variable_types()를 재사용함
# ============================================================================

# ----------------------------------------------------------------------------
# 1. 결측값 표준화
# ----------------------------------------------------------------------------
# 기본 결측값은 00_variable_type.R의 normalize_missing() 기준을 따른다.
# 단, 9, 99, 999 같은 값은 기본 결측값으로 자동 처리하지 않고,
# 사용자가 user_missing_codes에 지정한 경우에만 결측값으로 처리한다.
quality_normalize_missing <- function(x, user_missing_codes = NULL) {
  if (exists("normalize_missing", mode = "function")) {
    x <- normalize_missing(x)
  } else {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    if (is.character(x)) {
      x <- trimws(x)
      x[x %in% c("", "NA", "N/A", "na", "n/a", "null", "NULL")] <- NA
    }
  }
  
  if (!is.null(user_missing_codes) && length(user_missing_codes) > 0) {
    code_chr <- trimws(as.character(user_missing_codes))
    
    if (is.numeric(x) || is.integer(x)) {
      code_num <- suppressWarnings(as.numeric(user_missing_codes))
      code_num <- code_num[!is.na(code_num)]
      if (length(code_num) > 0) {
        x[x %in% code_num] <- NA
      }
    } else {
      x_chr <- trimws(as.character(x))
      x[x_chr %in% code_chr] <- NA
    }
  }
  
  return(x)
}

# 데이터프레임 전체에 결측값 표준화 적용
quality_standardize_data <- function(data, user_missing_codes = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  
  for (j in seq_along(data)) {
    data[[j]] <- quality_normalize_missing(data[[j]], user_missing_codes)
  }
  
  return(data)
}

# ----------------------------------------------------------------------------
# 2. 안전한 비율 계산
# ----------------------------------------------------------------------------
quality_rate <- function(numerator, denominator) {
  if (is.null(denominator) || is.na(denominator) || denominator <= 0) {
    return(0)
  }
  return(numerator / denominator)
}

quality_percent <- function(rate, digits = 2) {
  return(round(rate * 100, digits))
}

# ----------------------------------------------------------------------------
# 3. 점수 계산 함수
# ----------------------------------------------------------------------------
score_missing_total <- function(rate) {
  if (rate < 0.05) return(15)
  if (rate < 0.10) return(12)
  if (rate < 0.20) return(8)
  if (rate < 0.40) return(4)
  return(0)
}

score_missing_high_variable <- function(rate) {
  if (rate < 0.05) return(10)
  if (rate < 0.10) return(8)
  if (rate < 0.20) return(5)
  if (rate < 0.30) return(3)
  return(0)
}

score_type_issue <- function(rate) {
  if (rate < 0.05) return(15)
  if (rate < 0.10) return(12)
  if (rate < 0.20) return(8)
  if (rate < 0.30) return(4)
  return(0)
}

score_special_value <- function(rate) {
  if (rate == 0) return(5)
  if (rate < 0.05) return(4)
  if (rate < 0.10) return(3)
  if (rate < 0.20) return(1)
  return(0)
}

score_duplicate <- function(rate) {
  if (rate < 0.01) return(15)
  if (rate < 0.05) return(12)
  if (rate < 0.10) return(8)
  if (rate < 0.20) return(4)
  return(0)
}

score_outlier <- function(rate) {
  if (rate <= 0.05) return(15)
  if (rate <= 0.10) return(12)
  if (rate <= 0.20) return(8)
  if (rate <= 0.30) return(4)
  return(0)
}

score_variable_structure <- function(rate) {
  if (rate < 0.05) return(10)
  if (rate < 0.10) return(8)
  if (rate < 0.20) return(5)
  if (rate < 0.30) return(3)
  return(0)
}

score_categorical <- function(rate) {
  if (rate < 0.05) return(10)
  if (rate < 0.10) return(8)
  if (rate < 0.20) return(5)
  if (rate < 0.30) return(3)
  return(0)
}

# ----------------------------------------------------------------------------
# 4. 상태 판정 함수
# ----------------------------------------------------------------------------
quality_item_status <- function(score, max_score) {
  if (score >= max_score * 0.8) {
    return("양호")
  } else if (score >= max_score * 0.6) {
    return("주의")
  } else {
    return("위험")
  }
}

quality_final_status <- function(total_score, force_danger = FALSE) {
  if (isTRUE(force_danger)) {
    return("위험")
  }
  if (total_score >= 80) return("양호")
  if (total_score >= 60) return("주의")
  return("위험")
}

# ----------------------------------------------------------------------------
# 5. 출력 문구
# ----------------------------------------------------------------------------
quality_final_message <- function(status) {
  if (status == "양호") {
    return("전체 데이터 품질이 양호한 편입니다. 결측치, 중복값, 이상치, 자료형 오류 등 주요 품질 문제가 크지 않아 기본적인 분석과 시각화를 진행할 수 있습니다. 다만 일부 변수에서 작은 문제가 있을 수 있으므로 변수별 진단표를 확인한 뒤 필요한 경우 간단한 전처리를 수행하는 것이 좋습니다.")
  }
  if (status == "주의") {
    return("데이터 분석은 가능하지만 일부 품질 문제가 확인되었습니다. 결측치, 이상치, 중복값, 자료형 오류, 변수 구조 문제 등이 분석 결과에 영향을 줄 수 있으므로 바로 분석을 진행하기보다 주요 문제 항목을 확인하고 전처리를 수행하는 것이 권장됩니다.")
  }
  return("데이터 품질이 낮아 분석 결과의 신뢰성이 떨어질 가능성이 큽니다. 결측치, 중복값, 이상치, 자료형 오류 또는 변수 구조 문제가 크게 나타났을 수 있으므로 분석을 진행하기 전에 데이터 수집 과정과 입력 오류를 재검토하고 충분한 전처리를 수행해야 합니다.")
}

quality_issue_message <- function(item, status) {
  messages <- list(
    "결측치 품질" = list(
      "주의" = "결측치 품질에 주의가 필요합니다. 일부 변수 또는 전체 데이터에서 결측값이 확인되어 분석에 사용되는 관측치 수가 줄어들 수 있습니다. 결측률이 높은 변수를 확인하고, 삭제·대체·분석 제외 여부를 결정하는 것이 좋습니다.",
      "위험" = "결측치 품질이 낮은 상태입니다. 결측값이 많이 포함되어 있어 분석 결과가 일부 데이터에만 기반하거나 왜곡될 가능성이 있습니다. 분석을 진행하기 전에 결측 원인을 확인하고, 결측치 처리 방법을 우선적으로 결정해야 합니다."
    ),
    "자료형·형식 및 특수값 일관성" = list(
      "주의" = "자료형 및 형식 일관성에 주의가 필요합니다. 일부 변수가 숫자처럼 보이지만 문자형으로 저장되어 있거나, 하나의 변수 안에 여러 형식이 섞여 있을 가능성이 있습니다. 분석 전에 변수 자료형을 확인하고 필요한 경우 숫자형 또는 날짜형으로 변환해야 합니다.",
      "위험" = "자료형 및 형식 일관성 문제가 크게 나타났습니다. 여러 변수에서 자료형 오류가 의심되어 통계 계산, 그래프 생성, 모델링 과정에서 오류가 발생할 수 있습니다. 분석 전에 변수별 자료형을 재검토하고 형식 변환 작업을 우선적으로 수행해야 합니다."
    ),
    "중복값 품질" = list(
      "주의" = "중복값 품질에 주의가 필요합니다. 일부 동일한 행이 반복되어 포함되어 있을 가능성이 있습니다. 중복 행이 실제 반복 관측인지, 데이터 입력 또는 병합 과정에서 발생한 오류인지 확인한 뒤 필요하면 제거하는 것이 좋습니다.",
      "위험" = "중복값 문제가 많이 확인되었습니다. 동일한 관측치가 여러 번 포함되면 평균, 빈도, 비율, 모델 학습 결과가 왜곡될 수 있습니다. 분석 전에 중복 행의 원인을 확인하고, 실제 중복 오류라면 제거해야 합니다."
    ),
    "이상치 품질" = list(
      "주의" = "이상치 품질에 주의가 필요합니다. 일부 수치형 변수에서 일반적인 범위를 벗어난 값이 확인되었습니다. 이상치가 실제로 의미 있는 값인지, 입력 오류인지 확인하고 필요한 경우 별도로 처리하는 것이 좋습니다.",
      "위험" = "이상치 문제가 크게 나타났습니다. 여러 수치형 변수에서 극단적인 값이 많이 포함되어 평균, 표준편차, 그래프, 상관계수 등이 왜곡될 수 있습니다. 분석 전에 박스플롯이나 요약 통계를 통해 이상치 원인을 우선 확인해야 합니다."
    ),
    "변수 구조 품질" = list(
      "주의" = "변수 구조 품질에 주의가 필요합니다. 일부 변수는 값이 거의 변하지 않거나, 고유값이 지나치게 많아 ID성 변수일 가능성이 있습니다. 분석에 사용할 변수인지, 단순 식별자인지 확인한 뒤 필요하면 제외하는 것이 좋습니다.",
      "위험" = "변수 구조 품질이 낮은 상태입니다. 상수 변수, 분산이 없는 변수, ID성 변수로 의심되는 변수가 많이 포함되어 있을 수 있습니다. 이러한 변수들은 분석에 유용한 정보를 거의 제공하지 못하므로 변수 정리가 우선적으로 필요합니다."
    ),
    "범주형 변수 품질" = list(
      "주의" = "범주형 변수 품질에 주의가 필요합니다. 일부 범주형 변수에서 특정 범주에 데이터가 몰려 있거나, 범주의 종류가 지나치게 많을 가능성이 있습니다. 범주를 통합하거나 분석 대상 변수로 적절한지 확인하는 것이 좋습니다.",
      "위험" = "범주형 변수 품질이 낮은 상태입니다. 특정 범주에 데이터가 심하게 치우쳐 있거나, 범주 수가 표본 수에 비해 지나치게 많은 변수가 포함되어 있을 수 있습니다. 이 경우 빈도 분석이나 집단 비교 결과가 제한적으로 해석될 수 있으므로 범주 구조를 먼저 점검해야 합니다."
    ),
    "데이터 기본 구조 및 변수명 품질" = list(
      "주의" = "데이터 기본 구조에 주의가 필요합니다. 행 수가 적거나 변수 수가 부족하여 분석 결과를 일반화하기 어려울 수 있습니다. 간단한 데이터 확인은 가능하지만, 분석 결과 해석에는 주의가 필요합니다.",
      "위험" = "데이터 기본 구조가 분석에 적합하지 않은 상태입니다. 행 또는 열이 거의 없거나 분석 가능한 변수가 부족하여 품질 진단과 기본 분석을 수행하기 어렵습니다. 데이터 파일이 올바르게 업로드되었는지, 변수명이 제대로 인식되었는지 먼저 확인해야 합니다."
    )
  )
  
  if (!is.null(messages[[item]]) && !is.null(messages[[item]][[status]])) {
    return(messages[[item]][[status]])
  }
  
  return("해당 항목에서 품질 문제가 확인되었습니다. 세부 결과를 확인하고 필요한 전처리를 수행하는 것이 좋습니다.")
}

# ----------------------------------------------------------------------------
# 6. 변수 유형 관련 공통 함수
# ----------------------------------------------------------------------------
quality_numeric_category_types <- function() {
  return(c("숫자형 범주 변수", "수치형 범주 변수"))
}

quality_analysis_types <- function() {
  return(c("날짜형 변수", "범주형 변수", quality_numeric_category_types(), "수치형 변수"))
}

quality_analysis_variable_count <- function(variable_types) {
  if (is.null(variable_types) || nrow(variable_types) == 0) {
    return(0)
  }
  return(sum(variable_types$variable_type %in% quality_analysis_types()))
}

# 00_variable_type.R의 detect_all_variable_types()를 우선 사용한다.
quality_detect_variable_types <- function(data) {
  if (exists("detect_all_variable_types", mode = "function")) {
    return(detect_all_variable_types(data))
  }
  
  stop("detect_all_variable_types() 함수를 찾을 수 없습니다. R/suitability/00_variable_type.R을 먼저 source 해주세요.")
}
