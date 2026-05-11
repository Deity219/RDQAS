# ============================================================
# 3. 회귀 모델 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_regression_result <- function(analysis_name = "회귀 모델",
                                   essential_pass,
                                   score = 0,
                                   final_decision = "부적합",
                                   messages = character(),
                                   detail = NULL) {
  list(
    analysis = analysis_name,
    essential_pass = essential_pass,
    score = score,
    final_decision = final_decision,
    messages = messages,
    detail = detail
  )
}


# ------------------------------------------------------------
# 이상치 비율 계산 함수
# ------------------------------------------------------------
calc_outlier_rate_iqr <- function(x) {
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
}


# ------------------------------------------------------------
# 회귀 모델 적합성 평가 함수
# ------------------------------------------------------------
check_regression <- function(df, type_result, y_var = NULL, x_vars = NULL) {
  
  messages <- character()
  
  # ----------------------------------------------------------
  # 1. 반응변수 선택 여부 확인
  # ----------------------------------------------------------
  y_selected <- !is.null(y_var) &&
    length(y_var) == 1 &&
    !is.na(y_var) &&
    y_var %in% names(df)
  
  if (y_selected) {
    y_name <- y_var
    y_type <- type_result$variable_type[type_result$variable == y_name][1]
    y_raw <- df[[y_name]]
    y_norm <- normalize_missing(y_raw)
    y_missing_rate <- mean(is.na(y_norm))
    y_unique <- length(unique(y_norm[!is.na(y_norm)]))
    
    y_is_numeric <- is.numeric(y_raw) &&
      y_type %in% c("수치형 변수", "숫자형 범주 변수")
    
    y_nonmiss_vector <- !is.na(y_norm)
    
  } else {
    y_name <- NA_character_
    y_type <- NA_character_
    y_missing_rate <- NA_real_
    y_unique <- 0
    y_is_numeric <- FALSE
    y_nonmiss_vector <- rep(FALSE, nrow(df))
  }
  
  # ----------------------------------------------------------
  # 2. 설명변수 후보 설정
  # ----------------------------------------------------------
  if (is.null(x_vars) || length(x_vars) == 0) {
    if (y_selected) {
      x_candidates <- setdiff(names(df), y_name)
    } else {
      x_candidates <- names(df)
    }
  } else {
    x_candidates <- intersect(x_vars, names(df))
    
    if (y_selected) {
      x_candidates <- setdiff(x_candidates, y_name)
    }
  }
  
  x_candidates <- unique(x_candidates)
  
  # ----------------------------------------------------------
  # 3. 설명변수 후보별 모델링 가능 여부 판정
  # ----------------------------------------------------------
  if (length(x_candidates) == 0) {
    
    x_detail <- data.frame(
      variable = character(),
      variable_type = character(),
      n_non_missing = integer(),
      n_unique_non_missing = integer(),
      missing_rate = numeric(),
      is_id = logical(),
      is_single_value = logical(),
      is_missing_excess = logical(),
      is_modelable = logical(),
      modelable_role = character(),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    x_detail <- do.call(
      rbind,
      lapply(x_candidates, function(v) {
        
        x_raw <- df[[v]]
        x_norm <- normalize_missing(x_raw)
        x_nonmiss <- x_norm[!is.na(x_norm)]
        
        n_nonmiss <- length(x_nonmiss)
        n_unique <- length(unique(x_nonmiss))
        missing_rate <- mean(is.na(x_norm))
        unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
        
        var_type <- type_result$variable_type[type_result$variable == v][1]
        
        is_id <- n_nonmiss >= 30 && unique_ratio >= 0.9
        is_single_value <- n_unique < 2
        is_missing_excess <- missing_rate >= 0.80
        
        is_numeric_explanatory <- is.numeric(x_raw) &&
          n_unique >= 2 &&
          !(var_type %in% c("ID성 변수", "제외 변수", "날짜형 변수"))
        
        is_categorical_explanatory <- (is.character(x_raw) || is.factor(x_raw) || is.logical(x_raw)) &&
          n_unique >= 2 &&
          n_unique <= 20 &&
          !(var_type %in% c("ID성 변수", "제외 변수", "날짜형 변수"))
        
        is_modelable <- !is_single_value &&
          !is_id &&
          !is_missing_excess &&
          (is_numeric_explanatory || is_categorical_explanatory)
        
        modelable_role <- if (is_modelable && is_numeric_explanatory) {
          "수치형 설명변수"
        } else if (is_modelable && is_categorical_explanatory) {
          "범주형 설명변수"
        } else {
          "제외 후보"
        }
        
        data.frame(
          variable = v,
          variable_type = var_type,
          n_non_missing = n_nonmiss,
          n_unique_non_missing = n_unique,
          missing_rate = missing_rate,
          is_id = is_id,
          is_single_value = is_single_value,
          is_missing_excess = is_missing_excess,
          is_modelable = is_modelable,
          modelable_role = modelable_role,
          stringsAsFactors = FALSE
        )
      })
    )
  }
  
  modelable_x_vars <- x_detail$variable[x_detail$is_modelable]
  numeric_modelable_x_vars <- x_detail$variable[
    x_detail$is_modelable & x_detail$modelable_role == "수치형 설명변수"
  ]
  
  n_x_candidates <- length(x_candidates)
  n_modelable_x <- length(modelable_x_vars)
  
  # ----------------------------------------------------------
  # 4. 유효 관측치 계산
  # 유효 관측치 = 반응변수가 결측이 아니고,
  # 모델링 가능한 설명변수 중 하나 이상에 결측이 아닌 값이 존재하는 행
  # ----------------------------------------------------------
  if (y_selected && n_modelable_x > 0) {
    
    modelable_x_df <- df[, modelable_x_vars, drop = FALSE]
    
    modelable_x_df_norm <- as.data.frame(
      lapply(modelable_x_df, normalize_missing)
    )
    
    x_valid_rows <- rowSums(!is.na(modelable_x_df_norm)) > 0
    valid_rows <- y_nonmiss_vector & x_valid_rows
    n_valid <- sum(valid_rows)
    
  } else {
    valid_rows <- rep(FALSE, nrow(df))
    n_valid <- 0
  }
  
  # ----------------------------------------------------------
  # 5. 필수 기준: P/NP
  # ----------------------------------------------------------
  f1 <- y_selected
  f2 <- y_selected && y_is_numeric
  f3 <- n_x_candidates >= 1
  f4 <- n_valid >= 30
  f5 <- y_selected && y_unique >= 2
  f6 <- n_modelable_x >= 1
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3", "F4", "F5", "F6"),
    description = c(
      "반응변수가 1개 선택되어 있음",
      "선택한 반응변수가 수치형으로 해석 가능",
      "설명변수 후보가 1개 이상 존재",
      "반응변수와 설명변수 기준, 결측 제외 유효 관측치가 30개 이상 존재",
      "반응변수의 결측 제외 고유값 수가 2개 이상 존재",
      "모델링 가능한 설명변수가 1개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3, f4, f5, f6), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3, f4, f5, f6))
  
  # ----------------------------------------------------------
  # 6. 추가 경고 문구
  # ----------------------------------------------------------
  if (y_selected && y_unique >= 2 && y_unique <= 4) {
    messages <- c(
      messages,
      "반응변수의 고유값 수가 적어 회귀 모델보다 분류 모델이 더 적절할 수 있습니다."
    )
  }
  
  sample_variable_ratio <- ifelse(n_modelable_x > 0, n_valid / n_modelable_x, NA_real_)
  
  if (!is.na(sample_variable_ratio) && sample_variable_ratio < 3) {
    messages <- c(
      messages,
      "설명변수 수에 비해 표본 수가 적어 과적합 위험이 큽니다."
    )
  }
  
  if (y_selected && !is.na(y_missing_rate) && y_missing_rate >= 0.40) {
    messages <- c(
      messages,
      "반응변수에 결측치가 많아 회귀 모델 학습에 사용할 수 있는 표본이 크게 줄어들 수 있습니다."
    )
  }
  
  avg_missing_rate <- if (n_x_candidates > 0 && y_selected) {
    mean(c(y_missing_rate, x_detail$missing_rate))
  } else if (y_selected) {
    y_missing_rate
  } else {
    NA_real_
  }
  
  if (!is.na(avg_missing_rate) && avg_missing_rate >= 0.40) {
    messages <- c(
      messages,
      "주요 변수에 결측치가 많아 전처리 또는 변수 선택이 필요합니다."
    )
  }
  
  response_outlier_rate <- if (y_selected && y_is_numeric) {
    calc_outlier_rate_iqr(df[[y_name]])
  } else {
    NA_real_
  }
  
  if (!is.na(response_outlier_rate) && response_outlier_rate > 0.20) {
    messages <- c(
      messages,
      "반응변수의 극단값이 회귀 모델의 추정이나 예측 성능에 큰 영향을 줄 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준 미충족 시 최종 판정
  # ----------------------------------------------------------
  if (!essential_pass) {
    
    return(make_regression_result(
      analysis_name = "회귀 모델",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      detail = list(
        essential = essential_detail,
        selected_response = y_name,
        response_type = y_type,
        response_missing_rate = y_missing_rate,
        response_unique_count = y_unique,
        response_outlier_rate = response_outlier_rate,
        x_candidates = x_candidates,
        x_detail = x_detail,
        modelable_x_vars = modelable_x_vars,
        n_valid = n_valid,
        n_x_candidates = n_x_candidates,
        n_modelable_x = n_modelable_x,
        sample_variable_ratio = sample_variable_ratio,
        avg_missing_rate = avg_missing_rate
      )
    ))
  }
  
  # ==========================================================
  # 권장 기준 점수 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수의 충분성, 30점
  # ----------------------------------------------------------
  if (n_valid >= 200) {
    r1 <- 30
    r1_reason <- "200개 이상"
  } else if (n_valid >= 100) {
    r1 <- 24
    r1_reason <- "100개 이상 200개 미만"
  } else if (n_valid >= 50) {
    r1 <- 18
    r1_reason <- "50개 이상 100개 미만"
  } else if (n_valid >= 30) {
    r1 <- 10
    r1_reason <- "30개 이상 50개 미만"
  } else {
    r1 <- 0
    r1_reason <- "30개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 설명변수 수 대비 표본 수의 충분성, 20점
  # ----------------------------------------------------------
  if (sample_variable_ratio >= 10) {
    r2 <- 20
    r2_reason <- "10 이상"
  } else if (sample_variable_ratio >= 5) {
    r2 <- 15
    r2_reason <- "5 이상 10 미만"
  } else if (sample_variable_ratio >= 3) {
    r2 <- 8
    r2_reason <- "3 이상 5 미만"
  } else {
    r2 <- 3
    r2_reason <- "3 미만"
  }
  
  # ----------------------------------------------------------
  # R3. 반응변수의 품질, 20점
  # ----------------------------------------------------------
  if (y_missing_rate < 0.05 && y_unique >= 10) {
    r3 <- 20
    r3_reason <- "반응변수 결측률 5% 미만 + 고유값 수 10개 이상"
  } else if (y_missing_rate < 0.20 && y_unique >= 5) {
    r3 <- 15
    r3_reason <- "반응변수 결측률 20% 미만 + 고유값 수 5개 이상"
  } else if (y_missing_rate < 0.40 && y_unique >= 2) {
    r3 <- 8
    r3_reason <- "반응변수 결측률 40% 미만 + 고유값 수 2개 이상"
  } else {
    r3 <- 0
    r3_reason <- "반응변수 결측률 40% 이상"
  }
  
  # ----------------------------------------------------------
  # R4. 설명변수의 사용 가능성, 15점
  # ----------------------------------------------------------
  usable_x_ratio <- n_modelable_x / n_x_candidates
  
  if (usable_x_ratio >= 0.80) {
    r4 <- 15
    r4_reason <- "80% 이상"
  } else if (usable_x_ratio >= 0.50) {
    r4 <- 10
    r4_reason <- "50% 이상 80% 미만"
  } else if (usable_x_ratio >= 0.20) {
    r4 <- 5
    r4_reason <- "20% 이상 50% 미만"
  } else {
    r4 <- 0
    r4_reason <- "20% 미만"
  }
  
  # ----------------------------------------------------------
  # R5. 결측률 적정성, 10점
  # ----------------------------------------------------------
  if (avg_missing_rate < 0.05) {
    r5 <- 10
    r5_reason <- "5% 미만"
  } else if (avg_missing_rate < 0.20) {
    r5 <- 7
    r5_reason <- "5% 이상 20% 미만"
  } else if (avg_missing_rate < 0.40) {
    r5 <- 3
    r5_reason <- "20% 이상 40% 미만"
  } else {
    r5 <- 0
    r5_reason <- "40% 이상"
  }
  
  # ----------------------------------------------------------
  # R6. 이상치 영향 정도, 5점
  # ----------------------------------------------------------
  outlier_target_vars <- c(y_name, numeric_modelable_x_vars)
  
  outlier_rates <- sapply(outlier_target_vars, function(v) {
    calc_outlier_rate_iqr(df[[v]])
  })
  
  avg_outlier_rate <- mean(outlier_rates)
  
  if (avg_outlier_rate <= 0.10) {
    r6 <- 5
    r6_reason <- "10% 이하"
  } else if (avg_outlier_rate <= 0.20) {
    r6 <- 3
    r6_reason <- "10% 초과 20% 이하"
  } else {
    r6 <- 0
    r6_reason <- "20% 초과"
  }
  
  # ----------------------------------------------------------
  # 총점 및 최종 판정
  # ----------------------------------------------------------
  total_score <- r1 + r2 + r3 + r4 + r5 + r6
  
  if (total_score >= 80) {
    final_decision <- "적합"
  } else if (total_score >= 60) {
    final_decision <- "부분 적합"
  } else {
    final_decision <- "부적합"
  }
  
  # ----------------------------------------------------------
  # 권장 기준 세부 평가표
  # ----------------------------------------------------------
  score_detail <- data.frame(
    criterion = c("R1", "R2", "R3", "R4", "R5", "R6"),
    description = c(
      "유효 표본 수의 충분성",
      "설명변수 수 대비 표본 수의 충분성",
      "반응변수의 품질",
      "설명변수의 사용 가능성",
      "결측률 적정성",
      "이상치 영향 정도"
    ),
    score = c(r1, r2, r3, r4, r5, r6),
    max_score = c(30, 20, 20, 15, 10, 5),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason, r6_reason)
  )
  
  # ----------------------------------------------------------
  # 요약표
  # ----------------------------------------------------------
  regression_summary <- data.frame(
    item = c(
      "반응변수",
      "전체 설명변수 후보 수",
      "모델링 가능한 설명변수 수",
      "유효 관측치 수",
      "표본-변수 비율",
      "반응변수 결측률",
      "반응변수 고유값 수",
      "사용 가능한 설명변수 비율",
      "평균 결측률",
      "반응변수 이상치 비율",
      "평균 이상치 비율"
    ),
    value = c(
      y_name,
      n_x_candidates,
      n_modelable_x,
      n_valid,
      round(sample_variable_ratio, 4),
      round(y_missing_rate, 4),
      y_unique,
      round(usable_x_ratio, 4),
      round(avg_missing_rate, 4),
      round(response_outlier_rate, 4),
      round(avg_outlier_rate, 4)
    )
  )
  
  # ----------------------------------------------------------
  # 최종 결과 반환
  # ----------------------------------------------------------
  make_regression_result(
    analysis_name = "회귀 모델",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      regression_summary = regression_summary,
      selected_response = y_name,
      response_type = y_type,
      response_missing_rate = y_missing_rate,
      response_unique_count = y_unique,
      response_outlier_rate = response_outlier_rate,
      x_candidates = x_candidates,
      x_detail = x_detail,
      modelable_x_vars = modelable_x_vars,
      numeric_modelable_x_vars = numeric_modelable_x_vars,
      n_valid = n_valid,
      n_x_candidates = n_x_candidates,
      n_modelable_x = n_modelable_x,
      sample_variable_ratio = sample_variable_ratio,
      usable_x_ratio = usable_x_ratio,
      avg_missing_rate = avg_missing_rate,
      avg_outlier_rate = avg_outlier_rate
    )
  )
}