# ============================================================================
# 01_quality_common.R
# RDQAS 데이터 품질 검사 공통 함수
# ============================================================================
# [파일 역할]
# - R/quality 폴더의 여러 검사 파일에서 공통으로 사용하는 보조 함수를 모아둔다.
# - 결측값 표준화, 안전한 비율 계산, 점수 계산 기준, 판정 문구, 변수 유형 판정 연결,
#   컬럼별 요약표 생성, 향후 CSV/XLSX 업로드 보조 함수를 담당한다.
#
# [수정항목 반영 내역]
# - 수정항목 1) 품질 점수 계산 기준 보완
#   * 결측치 품질 계산에서 사용할 변수별 결측 패턴 점수 함수
#     score_missing_variable_pattern()을 추가했다.
#   * 기존에는 "결측률 40% 이상 변수 비율" 중심이었지만,
#     이제 최대 변수 결측률, 결측률 10%/20%/30% 이상 변수 수를 점수화할 수 있다.
#
# - 수정항목 3) 컬럼별 중복률 개념 정리
#   * 컬럼 내부 반복값을 "중복률"이라고 부르지 않도록
#     quality_create_column_summary()에서 고유값 수(unique_count)와
#     고유값 비율(unique_rate)만 제공한다.
#   * 행 단위 중복은 03_check_duplicates.R에서만 처리한다.
#
# - 수정항목 4) CSV 인코딩 및 한글 변수명 처리 보완
#   * 실제 업로드 UI/server 수정은 나중에 진행하지만,
#     quality_read_csv()와 quality_read_xlsx()를 향후 연결 가능한 보조 함수로 준비했다.
#   * 자동/UTF-8/UTF-8-BOM/CP949/EUC-KR 인코딩 선택 구조를 지원한다.
#
# - 수정항목 5) 빈 데이터/전체 결측 변수/상수 변수/중복 변수명 예외 처리
#   * quality_rate(), quality_detect_variable_types()에서 0행/0열 데이터가 들어와도
#     앱이 멈추지 않도록 안전하게 처리한다.
#
# - 수정항목 7) quality_report 반환 구조 정리
#   * 최종 결과에서 사용할 공통 판정 문구, 항목별 판정 문구, 주요 문제 항목 문구를
#     함수로 분리해 보고서 담당자가 재사용하기 쉽게 했다.
# ============================================================================


# ----------------------------------------------------------------------------
# 1. 결측값 표준화
# ----------------------------------------------------------------------------
# 기본 결측값은 00_variable_type.R의 normalize_missing() 기준을 우선 재사용한다.
# 9, 99, 999 같은 값은 기본 결측값으로 자동 처리하지 않는다.
# 나중에 UI/server에서 사용자가 별도로 지정한 경우에만 user_missing_codes로 받아 처리한다.
quality_normalize_missing <- function(x, user_missing_codes = NULL) {
  if (exists("normalize_missing", mode = "function")) {
    x <- normalize_missing(x)
  } else {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    
    if (is.character(x)) {
      x_trim <- trimws(x)
      x[x_trim %in% c("", "NA", "N/A", "NULL", "null")] <- NA
    }
  }
  
  # 향후 확장용:
  # 사용자가 "9, 99, 999" 등을 결측 코드로 지정한 경우에만 결측 처리한다.
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
# 2. 안전한 비율/퍼센트 계산
# ----------------------------------------------------------------------------
# 수정항목 5 반영:
# - 행 수 0개, 열 수 0개, 전체 셀 수 0개인 데이터가 들어와도
#   0으로 나누는 오류가 발생하지 않도록 한다.
quality_rate <- function(numerator, denominator) {
  if (is.null(denominator) || length(denominator) == 0 || is.na(denominator) || denominator <= 0) {
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

# 수정항목 1 반영:
# - 기존의 "결측률 40% 이상 변수 비율"만 보면 airquality의 Ozone처럼
#   특정 변수에 결측이 많이 몰린 경우를 충분히 반영하기 어렵다.
# - 따라서 변수별 최대 결측률과 10%/20%/30% 이상 결측 변수 수를 함께 반영한다.
# - 이 함수는 결측치 품질 25점 중 변수별 결측 패턴에 해당하는 10점을 계산한다.
score_missing_variable_pattern <- function(max_missing_rate,
                                           count_10,
                                           count_20,
                                           count_30) {
  score <- 10
  
  # 특정 변수 하나라도 결측률이 높으면 전체 결측률이 낮아도 일부 감점한다.
  if (max_missing_rate >= 0.30) {
    score <- score - 3
  } else if (max_missing_rate >= 0.20) {
    score <- score - 2
  } else if (max_missing_rate >= 0.10) {
    score <- score - 1
  }
  
  # 결측률 기준을 넘는 변수가 여러 개일수록 추가 감점한다.
  if (count_30 > 0) score <- score - 2
  if (count_20 > 0) score <- score - 2
  if (count_10 > 0) score <- score - 1
  
  return(max(score, 0))
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
# 4. 판정 함수
# ----------------------------------------------------------------------------
quality_item_status <- function(score, max_score) {
  rate <- quality_rate(score, max_score)
  
  if (rate >= 0.80) return("양호")
  if (rate >= 0.60) return("주의")
  return("위험")
}

quality_final_status <- function(total_score, force_danger = FALSE) {
  # 수정항목 5 반영:
  # 행/열/분석 가능한 변수가 없으면 점수와 관계없이 최종 판정을 위험으로 강제한다.
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

quality_item_message <- function(item, status) {
  messages <- list(
    "결측치 품질" = list(
      "양호" = "결측치 품질은 양호합니다. 전체적으로 결측값이 많지 않아 데이터의 기본 구조가 비교적 안정적입니다.",
      "주의" = "결측치 품질에 주의가 필요합니다. 일부 변수 또는 전체 데이터에서 결측값이 확인되어 분석에 사용되는 관측치 수가 줄어들 수 있습니다. 결측률이 높은 변수를 확인하고, 삭제·대체·분석 제외 여부를 결정하는 것이 좋습니다.",
      "위험" = "결측치 품질이 낮은 상태입니다. 결측값이 많이 포함되어 있어 분석 결과가 일부 데이터에만 기반하거나 왜곡될 가능성이 있습니다. 분석을 진행하기 전에 결측 원인을 확인하고, 결측치 처리 방법을 우선적으로 결정해야 합니다."
    ),
    "자료형·형식 및 특수값 일관성" = list(
      "양호" = "자료형 및 형식 일관성은 양호합니다. 대부분의 변수가 적절한 자료형으로 인식되며, 숫자·문자·날짜 형식의 혼재 문제가 크지 않습니다.",
      "주의" = "자료형 및 형식 일관성에 주의가 필요합니다. 일부 변수가 숫자처럼 보이지만 문자형으로 저장되어 있거나, 하나의 변수 안에 여러 형식이 섞여 있을 가능성이 있습니다. 분석 전에 변수 자료형을 확인하고 필요한 경우 숫자형 또는 날짜형으로 변환해야 합니다.",
      "위험" = "자료형 및 형식 일관성 문제가 크게 나타났습니다. 여러 변수에서 자료형 오류나 NaN/Inf/-Inf 값이 의심되어 통계 계산, 그래프 생성, 모델링 과정에서 오류가 발생할 수 있습니다."
    ),
    "중복값 품질" = list(
      "양호" = "중복값 품질은 양호합니다. 완전히 동일한 행의 비율이 낮아 중복 데이터로 인한 분석 왜곡 가능성은 크지 않습니다.",
      "주의" = "중복값 품질에 주의가 필요합니다. 일부 동일한 행이 반복되어 포함되어 있을 가능성이 있습니다. 중복 행이 실제 반복 관측인지, 데이터 입력 또는 병합 과정에서 발생한 오류인지 확인하는 것이 좋습니다.",
      "위험" = "중복값 문제가 많이 확인되었습니다. 동일한 관측치가 여러 번 포함되면 평균, 빈도, 비율, 모델 학습 결과가 왜곡될 수 있습니다. 분석 전에 중복 행의 원인을 확인해야 합니다."
    ),
    "이상치 품질" = list(
      "양호" = "이상치 품질은 양호합니다. 수치형 변수에서 일반적인 범위를 크게 벗어나는 값의 비율이 낮습니다.",
      "주의" = "이상치 품질에 주의가 필요합니다. 일부 수치형 변수에서 일반적인 범위를 벗어난 값이 확인되었습니다. 이상치가 실제로 의미 있는 값인지, 입력 오류인지 확인하는 것이 좋습니다.",
      "위험" = "이상치 문제가 크게 나타났습니다. 여러 수치형 변수에서 극단적인 값이 많이 포함되어 평균, 표준편차, 그래프, 상관계수 등이 왜곡될 수 있습니다."
    ),
    "변수 구조 품질" = list(
      "양호" = "변수 구조 품질은 양호합니다. 값이 하나뿐인 변수나 ID성 변수로 의심되는 변수가 많지 않습니다.",
      "주의" = "변수 구조 품질에 주의가 필요합니다. 일부 변수는 값이 거의 변하지 않거나, 고유값이 지나치게 많아 ID성 변수일 가능성이 있습니다.",
      "위험" = "변수 구조 품질이 낮은 상태입니다. 상수 변수, 분산이 없는 변수, ID성 변수로 의심되는 변수가 많이 포함되어 있을 수 있습니다. 변수 정리가 우선적으로 필요합니다."
    ),
    "범주형 변수 품질" = list(
      "양호" = "범주형 변수 품질은 양호합니다. 범주 수가 지나치게 많거나 특정 범주에 데이터가 과도하게 몰린 변수가 많지 않습니다.",
      "주의" = "범주형 변수 품질에 주의가 필요합니다. 일부 범주형 변수에서 특정 범주에 데이터가 몰려 있거나, 범주의 종류가 지나치게 많을 가능성이 있습니다.",
      "위험" = "범주형 변수 품질이 낮은 상태입니다. 특정 범주에 데이터가 심하게 치우쳐 있거나, 범주 수가 표본 수에 비해 지나치게 많은 변수가 포함되어 있을 수 있습니다."
    ),
    "데이터 기본 구조 및 변수명 품질" = list(
      "양호" = "데이터 기본 구조는 양호합니다. 행과 열이 충분히 존재하며, 기본적인 품질 진단과 요약 확인을 수행할 수 있는 형태입니다.",
      "주의" = "데이터 기본 구조에 주의가 필요합니다. 행 수가 적거나 변수 수가 부족하여 분석 결과를 일반화하기 어려울 수 있습니다.",
      "위험" = "데이터 기본 구조가 분석에 적합하지 않은 상태입니다. 행 또는 열이 거의 없거나 분석 가능한 변수가 부족하여 품질 진단과 기본 분석을 수행하기 어렵습니다."
    )
  )
  
  return(messages[[item]][[status]])
}


# ----------------------------------------------------------------------------
# 6. 변수 유형 판정 연결 함수
# ----------------------------------------------------------------------------
# 00_variable_type.R의 detect_all_variable_types()를 우선 사용한다.
# 수정항목 5 반영:
# - 0열 데이터나 변수 유형 판정 함수 오류가 발생해도 앱이 중단되지 않도록
#   최소한의 fallback 결과를 반환한다.
quality_detect_variable_types <- function(data) {
  if (ncol(data) == 0) {
    return(data.frame(
      variable = character(0),
      variable_type = character(0),
      non_missing_count = integer(0),
      unique_count = integer(0),
      unique_rate = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  if (exists("detect_all_variable_types", mode = "function")) {
    result <- tryCatch(
      detect_all_variable_types(data),
      error = function(e) NULL
    )
    
    if (!is.null(result)) {
      return(as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE))
    }
  }
  
  # fallback: 00_variable_type.R이 제대로 source되지 않았거나 오류가 난 경우
  variable_type <- vapply(data, function(x) {
    non_missing <- x[!is.na(x)]
    unique_count <- length(unique(non_missing))
    
    if (unique_count <= 1) return("제외 변수")
    if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("날짜형 변수")
    if (is.numeric(x) || is.integer(x)) {
      if (unique_count >= 11) return("수치형 변수")
      return("수치형 범주 변수")
    }
    return("범주형 변수")
  }, character(1))
  
  non_missing_count <- vapply(data, function(x) sum(!is.na(x)), integer(1))
  unique_count <- vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1))
  
  data.frame(
    variable = names(data),
    variable_type = variable_type,
    non_missing_count = non_missing_count,
    unique_count = unique_count,
    unique_rate = mapply(quality_rate, unique_count, non_missing_count),
    stringsAsFactors = FALSE
  )
}


# ----------------------------------------------------------------------------
# 7. 컬럼별 요약표 생성
# ----------------------------------------------------------------------------
# 수정항목 3 반영:
# - 컬럼 내부 반복값을 "중복률"이라고 표현하지 않는다.
# - 범주형 변수는 같은 값이 반복되는 것이 자연스럽기 때문에,
#   컬럼별 상세에는 고유값 수와 고유값 비율만 제공한다.
quality_create_column_summary <- function(data, variable_types) {
  if (ncol(data) == 0) {
    return(data.frame(
      variable = character(0),
      variable_type = character(0),
      missing_count = integer(0),
      missing_percent = numeric(0),
      non_missing_count = integer(0),
      unique_count = integer(0),
      unique_rate = numeric(0),
      stringsAsFactors = FALSE
    ))
  }
  
  missing_count <- vapply(data, function(x) sum(is.na(x)), integer(1))
  non_missing_count <- vapply(data, function(x) sum(!is.na(x)), integer(1))
  unique_count <- vapply(data, function(x) length(unique(x[!is.na(x)])), integer(1))
  
  data.frame(
    variable = names(data),
    variable_type = variable_types$variable_type,
    missing_count = missing_count,
    missing_percent = quality_percent(mapply(quality_rate, missing_count, nrow(data))),
    non_missing_count = non_missing_count,
    unique_count = unique_count,
    unique_rate = round(mapply(quality_rate, unique_count, non_missing_count), 4),
    stringsAsFactors = FALSE
  )
}


# ----------------------------------------------------------------------------
# 8. 향후 UI/server 연결용 파일 읽기 보조 함수
# ----------------------------------------------------------------------------
# 수정항목 4, 6 반영 준비:
# - 지금 작업 범위는 quality 폴더라서 실제 fileInput UI와 server 연결은 나중에 한다.
# - 다만 한글 CSV와 XLSX 처리를 위한 보조 함수를 여기에 준비해두면,
#   04_server.R 수정 시 그대로 재사용할 수 있다.
quality_read_csv <- function(file, encoding = c("auto", "UTF-8", "UTF-8-BOM", "CP949", "EUC-KR")) {
  encoding <- match.arg(encoding)
  
  encodings <- if (encoding == "auto") {
    c("UTF-8-BOM", "UTF-8", "CP949", "EUC-KR")
  } else {
    encoding
  }
  
  last_error <- NULL
  
  for (enc in encodings) {
    result <- tryCatch(
      read.csv(file, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = enc),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result)) {
      return(result)
    }
  }
  
  stop("CSV 파일을 읽지 못했습니다. UTF-8 또는 CP949 인코딩을 직접 선택해보세요. 원인: ", last_error$message)
}

quality_read_xlsx <- function(file, sheet = 1) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("XLSX 파일을 읽으려면 readxl 패키지가 필요합니다.")
  }
  
  return(as.data.frame(readxl::read_excel(file, sheet = sheet), stringsAsFactors = FALSE, check.names = FALSE))
}
