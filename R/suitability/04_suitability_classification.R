# ============================================================
# 4. 분류 모델 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_classification_result <- function(analysis_name = "분류 모델",
                                       essential_pass,
                                       score = 0,
                                       final_decision = "부적합",
                                       messages = character(),
                                       notes = character(),
                                       detail = NULL) {
  list(
    analysis = analysis_name,
    essential_pass = essential_pass,
    score = score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = detail
  )
}


# ------------------------------------------------------------
# 분류 모델 적합성 평가 함수
# ------------------------------------------------------------
check_classification <- function(df, type_result, y_var = NULL, x_vars = NULL) {
  
  messages <- character()
  notes <- character()
  
  # ----------------------------------------------------------
  # 1. 반응변수 선택 여부 확인
  # ----------------------------------------------------------
  y_selected <- !is.null(y_var) &&
    length(y_var) == 1 &&
    !is.na(y_var) &&
    y_var %in% names(df)
  
  if (y_selected) {
    y_name <- y_var
    y_raw <- df[[y_name]]
    y_norm <- normalize_missing(y_raw)
    
    y_type <- type_result$variable_type[type_result$variable == y_name][1]
    y_missing_rate <- mean(is.na(y_norm))
    y_unique_all <- length(unique(y_norm[!is.na(y_norm)]))
    
    y_is_categorical <- (
      y_type %in% c("범주형 변수", "숫자형 범주 변수") ||
        is.character(y_raw) ||
        is.factor(y_raw) ||
        is.logical(y_raw)
    )
    
    y_nonmiss_vector <- !is.na(y_norm)
    
  } else {
    y_name <- NA_character_
    y_raw <- NULL
    y_norm <- rep(NA, nrow(df))
    y_type <- NA_character_
    y_missing_rate <- NA_real_
    y_unique_all <- 0
    y_is_categorical <- FALSE
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
        
        is_numeric_explanatory <- var_type == "수치형 변수" &&
          n_unique >= 2
        
        is_categorical_explanatory <- var_type %in% c("범주형 변수", "숫자형 범주 변수") &&
          n_unique >= 2 &&
          n_unique <= 10
        
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
  
  categorical_modelable_x_vars <- x_detail$variable[
    x_detail$is_modelable & x_detail$modelable_role == "범주형 설명변수"
  ]
  
  n_x_candidates <- length(x_candidates)
  n_modelable_x <- length(modelable_x_vars)
  n_numeric_modelable_x <- length(numeric_modelable_x_vars)
  n_categorical_modelable_x <- length(categorical_modelable_x_vars)
  
  # ----------------------------------------------------------
  # 4. 유효 관측치 계산
  # 유효 관측치:
  # 반응변수가 결측이 아니고,
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
    
    y_valid <- y_norm[valid_rows]
    class_table <- table(y_valid)
    n_class <- length(class_table)
    min_class_count <- ifelse(n_class > 0, min(class_table), 0)
    class_ratio <- if (n_valid > 0) class_table / n_valid else numeric(0)
    min_class_ratio <- ifelse(length(class_ratio) > 0, min(class_ratio), NA_real_)
    
  } else {
    
    valid_rows <- rep(FALSE, nrow(df))
    n_valid <- 0
    y_valid <- character()
    class_table <- table(y_valid)
    n_class <- 0
    min_class_count <- 0
    class_ratio <- numeric(0)
    min_class_ratio <- NA_real_
  }
  
  minority_class_ratio <- if (n_class == 2) {
    min_class_ratio
  } else {
    NA_real_
  }
  
  sample_variable_ratio <- ifelse(n_modelable_x > 0, n_valid / n_modelable_x, NA_real_)
  
  avg_missing_rate <- if (n_x_candidates > 0 && y_selected) {
    mean(c(y_missing_rate, x_detail$missing_rate))
  } else if (y_selected) {
    y_missing_rate
  } else {
    NA_real_
  }
  
  usable_x_ratio <- ifelse(n_x_candidates > 0, n_modelable_x / n_x_candidates, NA_real_)
  
  # ----------------------------------------------------------
  # 5. 필수 기준: P/NP
  # ----------------------------------------------------------
  f1 <- y_selected
  f2 <- y_selected && y_is_categorical
  f3 <- n_class >= 2
  f4 <- n_x_candidates >= 1
  f5 <- n_valid >= 30
  f6 <- min_class_count >= 2
  f7 <- n_modelable_x >= 1
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3", "F4", "F5", "F6", "F7"),
    description = c(
      "반응변수가 1개 선택되어 있음",
      "선택한 반응변수가 범주형으로 해석 가능",
      "유효 관측치 기준, 반응변수의 클래스가 2개 이상 존재",
      "설명변수 후보가 1개 이상 존재",
      "반응변수와 설명변수 기준, 결측 제외 유효 관측치가 30개 이상 존재",
      "각 클래스에 최소 2개 이상의 유효 관측치가 존재",
      "모델링 가능한 설명변수가 1개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3, f4, f5, f6, f7), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3, f4, f5, f6, f7))
  
  # ----------------------------------------------------------
  # 6. 추가 안내 및 경고 문구
  # 같은 평가축에서 단계적으로 겹치는 경고는 가장 심한 문구 하나만 출력
  # 서로 다른 원인/평가축의 독립 경고는 모두 출력
  # ----------------------------------------------------------
  
  # 반응변수 클래스 수 관련 문구
  if (n_class > 10) {
    messages <- c(
      messages,
      "반응변수의 클래스 수가 많아 일반적인 분류 모델 학습과 해석이 어려울 수 있습니다. 클래스 통합이나 반응변수 재정의가 필요한지 확인하세요."
    )
  } else if (n_class >= 3) {
    messages <- c(
      messages,
      "다중 분류 문제로 판단됩니다. 다중 로지스틱 회귀, 분류나무, Random Forest 분류 등을 적용할 수 있습니다."
    )
  } else if (n_class == 2) {
    messages <- c(
      messages,
      "이진 분류 문제로 판단됩니다. 로지스틱 회귀, 이진 분류 트리, Random Forest 분류 등을 적용할 수 있습니다."
    )
  }
  
  # 최소 클래스 표본 수 관련 문구
  if (n_class >= 2 && min_class_count < 5) {
    messages <- c(
      messages,
      "일부 클래스의 표본 수가 매우 적어 분류 모델 학습과 평가가 불안정할 수 있습니다."
    )
  }
  
  # 이진 분류 클래스 불균형 관련 문구
  if (n_class == 2 && !is.na(minority_class_ratio) && minority_class_ratio < 0.10) {
    messages <- c(
      messages,
      "클래스 불균형이 커서 모델이 다수 클래스 위주로 예측할 가능성이 있습니다."
    )
  }
  
  # 다중 분류 클래스 불균형 관련 문구
  if (n_class >= 3 && !is.na(min_class_ratio) && min_class_ratio < 0.05) {
    messages <- c(
      messages,
      "특정 클래스의 표본이 부족하여 해당 클래스 예측 성능이 낮을 수 있습니다."
    )
  }
  
  # 표본-변수 비율 관련 문구
  if (!is.na(sample_variable_ratio) && sample_variable_ratio < 3) {
    messages <- c(
      messages,
      "설명변수 수에 비해 표본 수가 적어 과적합 위험이 큽니다."
    )
  }
  
  # 평균 결측률 관련 문구
  if (!is.na(avg_missing_rate) && avg_missing_rate >= 0.40) {
    messages <- c(
      messages,
      "주요 변수에 결측치가 많아 전처리 또는 변수 선택이 필요합니다."
    )
  }
  
  # ----------------------------------------------------------
  # 7. 참고 사항 문구
  # ----------------------------------------------------------
  if (n_numeric_modelable_x >= 1) {
    notes <- c(
      notes,
      "일부 분류 모델은 수치형 설명변수를 그대로 사용할 수 있지만, Naive Bayes를 적용하는 경우에는 방식에 따라 연속형 변수를 범주화하거나 Gaussian Naive Bayes처럼 연속형 변수를 처리할 수 있는 방법을 고려해야 합니다."
    )
  }
  
  if (n_categorical_modelable_x >= 1) {
    notes <- c(
      notes,
      "일부 분류 모델은 범주형 설명변수를 바로 사용할 수 없으므로, 모델 적용 전 더미변수화 또는 적절한 인코딩이 필요할 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준 미충족 시 최종 판정
  # ----------------------------------------------------------
  if (!essential_pass) {
    
    return(make_classification_result(
      analysis_name = "분류 모델",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      notes = notes,
      detail = list(
        essential = essential_detail,
        selected_response = y_name,
        response_type = y_type,
        response_missing_rate = y_missing_rate,
        response_unique_count = y_unique_all,
        x_candidates = x_candidates,
        x_detail = x_detail,
        modelable_x_vars = modelable_x_vars,
        numeric_modelable_x_vars = numeric_modelable_x_vars,
        categorical_modelable_x_vars = categorical_modelable_x_vars,
        n_valid = n_valid,
        class_table = class_table,
        n_class = n_class,
        min_class_count = min_class_count,
        min_class_ratio = min_class_ratio,
        minority_class_ratio = minority_class_ratio,
        n_x_candidates = n_x_candidates,
        n_modelable_x = n_modelable_x,
        sample_variable_ratio = sample_variable_ratio,
        usable_x_ratio = usable_x_ratio,
        avg_missing_rate = avg_missing_rate
      )
    ))
  }
  
  # ==========================================================
  # 권장 기준 점수 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수의 충분성, 25점
  # ----------------------------------------------------------
  if (n_valid >= 200) {
    r1 <- 25
    r1_reason <- "200개 이상"
  } else if (n_valid >= 100) {
    r1 <- 20
    r1_reason <- "100개 이상 200개 미만"
  } else if (n_valid >= 50) {
    r1 <- 15
    r1_reason <- "50개 이상 100개 미만"
  } else if (n_valid >= 30) {
    r1 <- 8
    r1_reason <- "30개 이상 50개 미만"
  } else {
    r1 <- 0
    r1_reason <- "30개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 클래스별 표본 수의 충분성, 20점
  # ----------------------------------------------------------
  if (min_class_count >= 20) {
    r2 <- 20
    r2_reason <- "20개 이상"
  } else if (min_class_count >= 10) {
    r2 <- 15
    r2_reason <- "10개 이상 20개 미만"
  } else if (min_class_count >= 5) {
    r2 <- 8
    r2_reason <- "5개 이상 10개 미만"
  } else if (min_class_count >= 2) {
    r2 <- 3
    r2_reason <- "2개 이상 5개 미만"
  } else {
    r2 <- 0
    r2_reason <- "2개 미만"
  }
  
  # ----------------------------------------------------------
  # R3. 클래스 불균형 정도, 20점
  # ----------------------------------------------------------
  if (n_class == 2) {
    
    if (minority_class_ratio >= 0.30) {
      r3 <- 20
      r3_reason <- "30% 이상"
    } else if (minority_class_ratio >= 0.20) {
      r3 <- 16
      r3_reason <- "20% 이상 30% 미만"
    } else if (minority_class_ratio >= 0.10) {
      r3 <- 10
      r3_reason <- "10% 이상 20% 미만"
    } else if (minority_class_ratio >= 0.05) {
      r3 <- 5
      r3_reason <- "5% 이상 10% 미만"
    } else {
      r3 <- 0
      r3_reason <- "5% 미만"
    }
    
  } else {
    
    if (min_class_ratio >= 0.15) {
      r3 <- 20
      r3_reason <- "15% 이상"
    } else if (min_class_ratio >= 0.10) {
      r3 <- 16
      r3_reason <- "10% 이상 15% 미만"
    } else if (min_class_ratio >= 0.05) {
      r3 <- 10
      r3_reason <- "5% 이상 10% 미만"
    } else if (min_class_ratio >= 0.02) {
      r3 <- 5
      r3_reason <- "2% 이상 5% 미만"
    } else {
      r3 <- 0
      r3_reason <- "2% 미만"
    }
  }
  
  # ----------------------------------------------------------
  # R4. 설명변수 수 대비 표본 수의 충분성, 15점
  # ----------------------------------------------------------
  if (sample_variable_ratio >= 10) {
    r4 <- 15
    r4_reason <- "10 이상"
  } else if (sample_variable_ratio >= 5) {
    r4 <- 11
    r4_reason <- "5 이상 10 미만"
  } else if (sample_variable_ratio >= 3) {
    r4 <- 6
    r4_reason <- "3 이상 5 미만"
  } else {
    r4 <- 2
    r4_reason <- "3 미만"
  }
  
  # ----------------------------------------------------------
  # R5. 설명변수의 사용 가능성, 10점
  # ----------------------------------------------------------
  if (usable_x_ratio >= 0.80) {
    r5 <- 10
    r5_reason <- "80% 이상"
  } else if (usable_x_ratio >= 0.50) {
    r5 <- 7
    r5_reason <- "50% 이상 80% 미만"
  } else if (usable_x_ratio >= 0.20) {
    r5 <- 3
    r5_reason <- "20% 이상 50% 미만"
  } else {
    r5 <- 0
    r5_reason <- "20% 미만"
  }
  
  # ----------------------------------------------------------
  # R6. 결측률 적정성, 10점
  # ----------------------------------------------------------
  if (avg_missing_rate < 0.05) {
    r6 <- 10
    r6_reason <- "5% 미만"
  } else if (avg_missing_rate < 0.20) {
    r6 <- 7
    r6_reason <- "5% 이상 20% 미만"
  } else if (avg_missing_rate < 0.40) {
    r6 <- 3
    r6_reason <- "20% 이상 40% 미만"
  } else {
    r6 <- 0
    r6_reason <- "40% 이상"
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
      "클래스별 표본 수의 충분성",
      "클래스 불균형 정도",
      "설명변수 수 대비 표본 수의 충분성",
      "설명변수의 사용 가능성",
      "결측률 적정성"
    ),
    score = c(r1, r2, r3, r4, r5, r6),
    max_score = c(25, 20, 20, 15, 10, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason, r6_reason)
  )
  
  # ----------------------------------------------------------
  # 요약표
  # ----------------------------------------------------------
  classification_summary <- data.frame(
    item = c(
      "반응변수",
      "전체 설명변수 후보 수",
      "모델링 가능한 설명변수 수",
      "수치형 설명변수 수",
      "범주형 설명변수 수",
      "유효 관측치 수",
      "반응변수 클래스 수",
      "최소 클래스 표본 수",
      "최소 클래스 비율",
      "표본-변수 비율",
      "사용 가능한 설명변수 비율",
      "평균 결측률"
    ),
    value = c(
      y_name,
      n_x_candidates,
      n_modelable_x,
      n_numeric_modelable_x,
      n_categorical_modelable_x,
      n_valid,
      n_class,
      min_class_count,
      round(min_class_ratio, 4),
      round(sample_variable_ratio, 4),
      round(usable_x_ratio, 4),
      round(avg_missing_rate, 4)
    )
  )
  
  # ----------------------------------------------------------
  # 최종 결과 반환
  # ----------------------------------------------------------
  make_classification_result(
    analysis_name = "분류 모델",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      classification_summary = classification_summary,
      selected_response = y_name,
      response_type = y_type,
      response_missing_rate = y_missing_rate,
      response_unique_count = y_unique_all,
      x_candidates = x_candidates,
      x_detail = x_detail,
      modelable_x_vars = modelable_x_vars,
      numeric_modelable_x_vars = numeric_modelable_x_vars,
      categorical_modelable_x_vars = categorical_modelable_x_vars,
      n_valid = n_valid,
      class_table = class_table,
      class_ratio = class_ratio,
      n_class = n_class,
      min_class_count = min_class_count,
      min_class_ratio = min_class_ratio,
      minority_class_ratio = minority_class_ratio,
      n_x_candidates = n_x_candidates,
      n_modelable_x = n_modelable_x,
      n_numeric_modelable_x = n_numeric_modelable_x,
      n_categorical_modelable_x = n_categorical_modelable_x,
      sample_variable_ratio = sample_variable_ratio,
      usable_x_ratio = usable_x_ratio,
      avg_missing_rate = avg_missing_rate
    )
  )
}