# ============================================================
# 1. 데이터 탐색(EDA) 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 공통 결과 반환 함수
# ------------------------------------------------------------
make_result <- function(analysis_name,
                        pass,
                        score = 0,
                        messages = character(),
                        notes = character(),
                        detail = NULL) {
  list(
    analysis = analysis_name,
    pass = pass,
    result = ifelse(pass, "Pass", "NonPass"),
    score = score,
    messages = messages,
    notes = notes,
    detail = detail
  )
}


# ------------------------------------------------------------
# EDA 적합성 평가 함수
# ------------------------------------------------------------
check_eda <- function(df, type_result, selected_vars = NULL) {
  
  # ----------------------------------------------------------
  # 0. 선택 변수 처리
  # ----------------------------------------------------------
  if (is.null(selected_vars)) {
    selected_vars <- names(df)
  }
  
  selected_vars <- selected_vars[selected_vars %in% names(df)]
  selected_type <- type_result[type_result$variable %in% selected_vars, ]
  
  # ----------------------------------------------------------
  # 1. EDA에서 분석 가능한 변수 유형 정의
  # ----------------------------------------------------------
  analyzable_types <- c(
    "수치형 변수",
    "범주형 변수",
    "숫자형 범주 변수"
  )
  
  analyzable_vars <- selected_type$variable[
    selected_type$variable_type %in% analyzable_types
  ]
  
  numeric_vars <- selected_type$variable[
    selected_type$variable_type == "수치형 변수"
  ]
  
  categorical_vars <- selected_type$variable[
    selected_type$variable_type %in% c("범주형 변수", "숫자형 범주 변수")
  ]
  
  n_numeric <- length(numeric_vars)
  n_categorical <- length(categorical_vars)
  
  messages <- character()
  notes <- c(
    "EDA 적합성 점수는 데이터가 탐색적 분석에 사용할 수 있는 기본 구조를 갖추었는지 평가한 점수이며, 특정 통계모형이나 검정의 적합성을 의미하지는 않습니다."
  )
  
  # ----------------------------------------------------------
  # 참고 사항 문구 생성
  # ----------------------------------------------------------
  if (n_numeric > 0 && n_categorical > 0) {
    notes <- c(
      notes,
      "수치형 변수의 요약 통계와 분포, 범주형 변수의 빈도와 비율, 변수 간 관계를 함께 확인할 수 있습니다."
    )
  } else if (n_numeric == 0 && n_categorical > 0) {
    notes <- c(
      notes,
      "수치형 변수가 없더라도 범주형 변수의 빈도표와 막대그래프 중심의 탐색은 가능합니다."
    )
  } else if (n_categorical == 0 && n_numeric > 0) {
    notes <- c(
      notes,
      "범주형 변수가 없더라도 수치형 변수의 요약 통계, 히스토그램, 박스플롯, 산점도 중심의 탐색은 가능합니다."
    )
  }
  
  # ----------------------------------------------------------
  # F1. 수치형 또는 범주형으로 해석 가능한 변수가 1개 이상 존재
  # ----------------------------------------------------------
  if (length(analyzable_vars) < 1) {
    
    essential_detail <- data.frame(
      criterion = c("F1", "F2"),
      description = c(
        "수치형 또는 범주형으로 해석 가능한 변수가 1개 이상 존재",
        "분석 가능한 변수 기준, 결측을 제외한 유효 관측치가 10개 이상 존재"
      ),
      result = c("NonPass", "Not evaluated")
    )
    
    return(make_result(
      analysis_name = "데이터 탐색(EDA)",
      pass = FALSE,
      score = 0,
      messages = c(
        "데이터 탐색에 사용할 수 있는 수치형 또는 범주형 변수가 없어 EDA를 수행하기 어렵습니다. 변수 형식이나 데이터 입력 상태를 확인하세요."
      ),
      notes = notes,
      detail = list(
        essential = essential_detail,
        selected_type = selected_type,
        analyzable_vars = analyzable_vars,
        numeric_vars = numeric_vars,
        categorical_vars = categorical_vars,
        n_numeric = n_numeric,
        n_categorical = n_categorical
      )
    ))
  }
  
  # ----------------------------------------------------------
  # 2. 유효 관측치 계산
  # EDA는 변수별 탐색이 가능하므로,
  # 분석 가능한 변수 중 하나라도 값이 있는 행을 유효 관측치로 정의
  # ----------------------------------------------------------
  analyzable_df <- df[, analyzable_vars, drop = FALSE]
  
  analyzable_df_norm <- as.data.frame(
    lapply(analyzable_df, normalize_missing)
  )
  
  valid_rows <- rowSums(!is.na(analyzable_df_norm)) > 0
  n_valid <- sum(valid_rows)
  
  # ----------------------------------------------------------
  # F2. 분석 가능한 변수 기준, 결측 제외 유효 관측치 10개 이상
  # ----------------------------------------------------------
  if (n_valid < 10) {
    
    essential_detail <- data.frame(
      criterion = c("F1", "F2"),
      description = c(
        "수치형 또는 범주형으로 해석 가능한 변수가 1개 이상 존재",
        "분석 가능한 변수 기준, 결측을 제외한 유효 관측치가 10개 이상 존재"
      ),
      result = c("Pass", "NonPass")
    )
    
    return(make_result(
      analysis_name = "데이터 탐색(EDA)",
      pass = FALSE,
      score = 0,
      messages = c(
        "분석 가능한 관측치 수가 너무 적어 데이터의 전반적인 구조나 분포를 안정적으로 파악하기 어렵습니다."
      ),
      notes = notes,
      detail = list(
        essential = essential_detail,
        selected_type = selected_type,
        analyzable_vars = analyzable_vars,
        numeric_vars = numeric_vars,
        categorical_vars = categorical_vars,
        n_valid = n_valid,
        n_numeric = n_numeric,
        n_categorical = n_categorical
      )
    ))
  }
  
  # ----------------------------------------------------------
  # 필수 기준 통과
  # ----------------------------------------------------------
  essential_detail <- data.frame(
    criterion = c("F1", "F2"),
    description = c(
      "수치형 또는 범주형으로 해석 가능한 변수가 1개 이상 존재",
      "분석 가능한 변수 기준, 결측을 제외한 유효 관측치가 10개 이상 존재"
    ),
    result = c("Pass", "Pass")
  )
  
  # ==========================================================
  # 권장 기준 배점 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 표본 수의 충분성, 35점
  # ----------------------------------------------------------
  if (n_valid >= 50) {
    r1 <- 35
    r1_reason <- "유효 관측치 50개 이상"
  } else if (n_valid >= 30) {
    r1 <- 28
    r1_reason <- "유효 관측치 30개 이상 50개 미만"
  } else if (n_valid >= 20) {
    r1 <- 20
    r1_reason <- "유효 관측치 20개 이상 30개 미만"
  } else if (n_valid >= 10) {
    r1 <- 10
    r1_reason <- "유효 관측치 10개 이상 20개 미만"
  } else if (n_valid >= 2) {
    r1 <- 3
    r1_reason <- "유효 관측치 2개 이상 10개 미만"
  } else {
    r1 <- 0
    r1_reason <- "유효 관측치 2개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 수치형 변수의 풍부성, 20점
  # ----------------------------------------------------------
  if (n_numeric >= 3 && n_valid >= n_numeric * 5) {
    r2 <- 20
    r2_reason <- "수치형 변수 3개 이상이고, 유효 관측치 수가 수치형 변수 수 × 5 이상"
  } else if (n_numeric >= 3 && n_valid < n_numeric * 5) {
    r2 <- 16
    r2_reason <- "수치형 변수 3개 이상이지만, 유효 관측치 수가 수치형 변수 수 × 5 미만"
  } else if (n_numeric == 2) {
    r2 <- 12
    r2_reason <- "수치형 변수 2개"
  } else if (n_numeric == 1) {
    r2 <- 6
    r2_reason <- "수치형 변수 1개"
  } else {
    r2 <- 0
    r2_reason <- "수치형 변수 없음"
  }
  
  # ----------------------------------------------------------
  # R3. 범주형 변수의 풍부성, 20점
  # 숫자형 범주 변수는 범주형 변수로 해석
  # ----------------------------------------------------------
  if (n_categorical >= 3) {
    r3 <- 20
    r3_reason <- "범주형 변수 3개 이상"
  } else if (n_categorical == 2) {
    r3 <- 15
    r3_reason <- "범주형 변수 2개"
  } else if (n_categorical == 1) {
    r3 <- 8
    r3_reason <- "범주형 변수 1개"
  } else {
    r3 <- 0
    r3_reason <- "범주형 변수 없음"
  }
  
  # ----------------------------------------------------------
  # R4. 결측률 적정성, 20점
  # ----------------------------------------------------------
  missing_rates <- sapply(analyzable_df_norm, function(x) {
    mean(is.na(x))
  })
  
  avg_missing_rate <- mean(missing_rates)
  
  if (avg_missing_rate < 0.05) {
    r4 <- 20
    r4_reason <- "분석 가능한 변수의 평균 결측률 5% 미만"
  } else if (avg_missing_rate < 0.20) {
    r4 <- 15
    r4_reason <- "평균 결측률 5% 이상 20% 미만"
  } else if (avg_missing_rate < 0.40) {
    r4 <- 8
    r4_reason <- "평균 결측률 20% 이상 40% 미만"
  } else if (avg_missing_rate < 0.60) {
    r4 <- 3
    r4_reason <- "평균 결측률 40% 이상 60% 미만"
  } else {
    r4 <- 0
    r4_reason <- "평균 결측률 60% 이상"
  }
  
  # ----------------------------------------------------------
  # R5. 이상치 영향 정도, 5점
  # ----------------------------------------------------------
  if (n_numeric == 0) {
    
    avg_outlier_rate <- NA
    r5 <- 5
    r5_reason <- "수치형 변수가 없는 경우"
    
  } else {
    
    outlier_rates <- sapply(df[, numeric_vars, drop = FALSE], function(x) {
      
      x <- normalize_missing(x)
      x <- x[!is.na(x)]
      
      if (length(x) < 2) {
        return(0)
      }
      
      q1 <- as.numeric(quantile(x, 0.25, na.rm = TRUE))
      q3 <- as.numeric(quantile(x, 0.75, na.rm = TRUE))
      iqr_value <- q3 - q1
      
      if (iqr_value == 0) {
        return(0)
      }
      
      lower <- q1 - 1.5 * iqr_value
      upper <- q3 + 1.5 * iqr_value
      
      mean(x < lower | x > upper)
    })
    
    avg_outlier_rate <- mean(outlier_rates)
    
    if (avg_outlier_rate <= 0.10) {
      r5 <- 5
      r5_reason <- "수치형 변수의 IQR 기준 평균 이상치 비율 10% 이하"
    } else if (avg_outlier_rate <= 0.20) {
      r5 <- 3
      r5_reason <- "평균 이상치 비율 10% 초과 20% 이하"
    } else {
      r5 <- 0
      r5_reason <- "평균 이상치 비율 20% 초과"
    }
  }
  
  # ----------------------------------------------------------
  # 총점 계산
  # ----------------------------------------------------------
  total_score <- r1 + r2 + r3 + r4 + r5
  
  # ----------------------------------------------------------
  # 권장 기준 세부 평가표
  # ----------------------------------------------------------
  score_detail <- data.frame(
    criterion = c("R1", "R2", "R3", "R4", "R5"),
    description = c(
      "표본 수의 충분성",
      "수치형 변수의 풍부성",
      "범주형 변수의 풍부성",
      "결측률 적정성",
      "이상치 영향 정도"
    ),
    score = c(r1, r2, r3, r4, r5),
    max_score = c(35, 20, 20, 20, 5),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason)
  )
  
  # ==========================================================
  # 추가 안내 및 경고 문구 생성
  # 같은 평가축에서 단계적으로 겹치는 경우 가장 심한 문구 하나만 출력
  # 서로 다른 원인/평가축의 독립 경고는 모두 출력
  # ==========================================================
  
  # 표본 수 관련 문구
  if (n_valid >= 10 && n_valid < 20) {
    messages <- c(
      messages,
      "EDA는 가능하지만 표본 수가 적어 요약 통계, 빈도, 시각화 결과가 데이터의 일반적인 패턴을 충분히 반영하지 못할 수 있습니다."
    )
  }
  
  # 변수 유형 부재 관련 문구
  if (n_numeric == 0) {
    messages <- c(
      messages,
      "수치형 요약 통계, 분포 확인, 이상치 탐색, 상관관계 확인은 제한되며 범주형 변수의 빈도와 비율 중심으로 탐색할 수 있습니다."
    )
  }
  
  if (n_categorical == 0) {
    messages <- c(
      messages,
      "범주별 빈도와 비율 확인은 제한되며 수치형 변수의 요약 통계, 분포, 이상치 탐색 중심으로 EDA를 수행할 수 있습니다."
    )
  }
  
  # 변수 개수 제한 관련 문구
  if (n_numeric <= 1 && n_categorical <= 1) {
    messages <- c(
      messages,
      "변수 간 관계나 집단별 패턴을 탐색하는 데 제한이 있을 수 있으며, 단일 변수 중심의 요약과 분포 확인에 가까운 EDA가 됩니다."
    )
  }
  
  # 수치형 변수 수 대비 관측치 수 관련 문구
  if (n_numeric >= 3 && n_valid < n_numeric * 5) {
    messages <- c(
      messages,
      "수치형 변수 수에 비해 관측치 수가 적어 여러 변수 간 관계를 해석할 때 결과가 불안정할 수 있습니다."
    )
  }
  
  # 결측률 관련 문구
  if (avg_missing_rate >= 0.60) {
    messages <- c(
      messages,
      "결측치가 매우 많아 EDA 결과의 신뢰성이 낮을 수 있습니다. 주요 변수의 입력 상태를 먼저 확인하는 것이 필요합니다."
    )
  } else if (avg_missing_rate >= 0.40) {
    messages <- c(
      messages,
      "결측치 비율이 높아 일부 변수의 분포나 관계를 해석하는 데 제한이 큽니다. 결측 처리 또는 변수 제외를 고려해야 합니다."
    )
  } else if (avg_missing_rate >= 0.20) {
    messages <- c(
      messages,
      "분석 대상 변수에 결측치가 많아 요약 통계, 빈도, 시각화 결과가 실제 데이터 구조를 충분히 반영하지 못할 수 있습니다."
    )
  }
  
  # 이상치 관련 문구
  if (!is.na(avg_outlier_rate)) {
    if (avg_outlier_rate > 0.20) {
      messages <- c(
        messages,
        "이상치 비율이 높아 수치형 변수의 분포와 요약 통계가 크게 왜곡될 수 있습니다. 이상치 원인 확인이 필요합니다."
      )
    } else if (avg_outlier_rate > 0.10) {
      messages <- c(
        messages,
        "일부 수치형 변수에 극단값이 포함되어 있어 평균, 분포 그래프, 상관관계 해석이 왜곡될 수 있습니다."
      )
    }
  }
  
  # ----------------------------------------------------------
  # 변수 요약표
  # ----------------------------------------------------------
  variable_summary <- data.frame(
    item = c(
      "선택 변수 수",
      "분석 가능한 변수 수",
      "수치형 변수 수",
      "범주형 변수 수",
      "유효 관측치 수",
      "평균 결측률",
      "평균 이상치 비율"
    ),
    value = c(
      length(selected_vars),
      length(analyzable_vars),
      n_numeric,
      n_categorical,
      n_valid,
      round(avg_missing_rate, 4),
      ifelse(is.na(avg_outlier_rate), NA, round(avg_outlier_rate, 4))
    )
  )
  
  # ----------------------------------------------------------
  # 최종 결과 반환
  # ----------------------------------------------------------
  make_result(
    analysis_name = "데이터 탐색(EDA)",
    pass = TRUE,
    score = total_score,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      variable_summary = variable_summary,
      selected_type = selected_type,
      analyzable_vars = analyzable_vars,
      numeric_vars = numeric_vars,
      categorical_vars = categorical_vars,
      n_valid = n_valid,
      n_numeric = n_numeric,
      n_categorical = n_categorical,
      avg_missing_rate = avg_missing_rate,
      avg_outlier_rate = avg_outlier_rate,
      missing_rates = missing_rates
    )
  )
}