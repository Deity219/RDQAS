# ============================================================
# 6. 생존 분석 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_survival_result <- function(analysis_name = "생존 분석",
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
# 변수 유형 가져오기 함수
# ------------------------------------------------------------
get_survival_variable_type <- function(type_result, var_name) {
  result <- type_result$variable_type[type_result$variable == var_name]
  
  if (length(result) == 0) {
    return(NA_character_)
  }
  
  return(result[1])
}


# ------------------------------------------------------------
# 생존 시간 변수 변환 함수
# ------------------------------------------------------------
convert_survival_time <- function(x) {
  
  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    return(rep(NA_real_, length(x)))
  }
  
  x <- normalize_missing(x)
  
  if (is.numeric(x) || is.integer(x)) {
    return(as.numeric(x))
  }
  
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  if (is.character(x)) {
    x_chr <- trimws(x)
    x_num <- suppressWarnings(as.numeric(x_chr))
    x_num[is.na(x)] <- NA_real_
    return(x_num)
  }
  
  return(rep(NA_real_, length(x)))
}


# ------------------------------------------------------------
# 생존 시간 변수 인정 여부 판정 함수
# ------------------------------------------------------------
is_valid_survival_time <- function(x) {
  x_num <- convert_survival_time(x)
  x_raw_norm <- normalize_missing(x)
  nonmiss_raw <- !is.na(x_raw_norm)
  
  if (sum(nonmiss_raw) == 0) {
    return(FALSE)
  }
  
  if (any(is.na(x_num[nonmiss_raw]))) {
    return(FALSE)
  }
  
  if (any(x_num[nonmiss_raw] < 0)) {
    return(FALSE)
  }
  
  return(TRUE)
}


# ------------------------------------------------------------
# 설명변수 후보별 사용 가능 여부 판정 함수
# ------------------------------------------------------------
make_survival_x_detail <- function(df, type_result, x_candidates) {
  
  if (length(x_candidates) == 0) {
    return(data.frame(
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
    ))
  }
  
  do.call(
    rbind,
    lapply(x_candidates, function(v) {
      
      x_raw <- df[[v]]
      x_norm <- normalize_missing(x_raw)
      x_nonmiss <- x_norm[!is.na(x_norm)]
      
      n_nonmiss <- length(x_nonmiss)
      n_unique <- length(unique(x_nonmiss))
      missing_rate <- mean(is.na(x_norm))
      unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
      
      var_type <- get_survival_variable_type(type_result, v)
      
      is_id <- n_nonmiss >= 30 && unique_ratio >= 0.9
      is_single_value <- n_unique < 2
      is_missing_excess <- missing_rate >= 0.80
      
      is_numeric_explanatory <- var_type == "수치형 변수" && n_unique >= 2
      
      is_categorical_explanatory <- var_type %in% c("범주형 변수", "숫자형 범주 변수") &&
        n_unique >= 2
      
      is_modelable <- !is_single_value &&
        !is_id &&
        !is_missing_excess &&
        !(var_type %in% c("제외 변수", "ID성 변수", "날짜형 변수")) &&
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


# ------------------------------------------------------------
# 생존 분석 적합성 평가 함수
# ------------------------------------------------------------
check_survival <- function(df,
                           type_result,
                           time_var = NULL,
                           event_var = NULL,
                           event_value = NULL,
                           single_event_is_event = NULL,
                           x_vars = NULL,
                           cox_model = FALSE) {
  
  messages <- character()
  notes <- character()
  
  notes <- c(
    notes,
    "생존 분석에서는 생존 시간 변수와 사건 발생 여부 변수가 핵심이며, 설명변수는 관련 요인 분석이나 Cox 모형을 수행할 때 추가로 사용됩니다."
  )
  
  # ----------------------------------------------------------
  # 1. 생존 시간 변수 선택 여부 및 변환
  # ----------------------------------------------------------
  time_selected <- !is.null(time_var) &&
    length(time_var) == 1 &&
    !is.na(time_var) &&
    time_var %in% names(df)
  
  if (time_selected) {
    time_raw <- df[[time_var]]
    time_type <- get_survival_variable_type(type_result, time_var)
    time_num <- convert_survival_time(time_raw)
    time_norm <- normalize_missing(time_raw)
    time_valid <- is_valid_survival_time(time_raw)
    time_missing_rate <- mean(is.na(time_norm))
    time_unique_count <- length(unique(time_num[!is.na(time_num)]))
    
    if (inherits(time_raw, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
      notes <- c(
        notes,
        "날짜 변수 자체는 사건 발생까지의 시간이 아니므로, 시작일과 종료일이 있다면 두 날짜의 차이를 이용해 추적 기간 변수를 먼저 만들어야 합니다."
      )
    }
    
  } else {
    time_raw <- NULL
    time_type <- NA_character_
    time_num <- rep(NA_real_, nrow(df))
    time_norm <- rep(NA, nrow(df))
    time_valid <- FALSE
    time_missing_rate <- NA_real_
    time_unique_count <- 0
  }
  
  # ----------------------------------------------------------
  # 2. 사건 발생 여부 변수 선택 여부 및 고유값 확인
  # ----------------------------------------------------------
  event_selected <- !is.null(event_var) &&
    length(event_var) == 1 &&
    !is.na(event_var) &&
    event_var %in% names(df)
  
  if (event_selected) {
    event_raw <- df[[event_var]]
    event_type <- get_survival_variable_type(type_result, event_var)
    event_norm <- normalize_missing(event_raw)
    event_chr <- as.character(event_norm)
    event_missing_rate <- mean(is.na(event_norm))
    event_unique_values <- unique(event_chr[!is.na(event_chr)])
    event_unique_count <- length(event_unique_values)
  } else {
    event_raw <- NULL
    event_type <- NA_character_
    event_norm <- rep(NA, nrow(df))
    event_chr <- rep(NA_character_, nrow(df))
    event_missing_rate <- NA_real_
    event_unique_values <- character()
    event_unique_count <- 0
  }
  
  if (event_unique_count == 1) {
    messages <- c(
      messages,
      "사건 발생 여부 변수에 하나의 상태값만 존재합니다. 모든 관측치가 사건 발생 또는 중도절단 중 하나로만 기록된 자료일 수 있으므로 해석에 주의가 필요합니다."
    )
  } else if (event_unique_count >= 3) {
    messages <- c(
      messages,
      "기본 생존 분석에는 바로 사용하기 어렵습니다. 사건 발생과 중도절단을 구분하는 이진 변수로 재코딩이 필요합니다."
    )
  }
  
  # ----------------------------------------------------------
  # 3. 사건 발생값 처리
  # ----------------------------------------------------------
  event_confirmed <- FALSE
  event_indicator <- rep(NA, nrow(df))
  censored_indicator <- rep(NA, nrow(df))
  
  if (event_selected && event_unique_count == 2) {
    
    event_value_chr <- as.character(event_value)
    
    if (!is.null(event_value) &&
        length(event_value) == 1 &&
        !is.na(event_value_chr) &&
        event_value_chr %in% event_unique_values) {
      
      event_confirmed <- TRUE
      event_indicator <- event_chr == event_value_chr
      censored_indicator <- !is.na(event_chr) & event_chr != event_value_chr
    }
    
  } else if (event_selected && event_unique_count == 1) {
    
    if (!is.null(single_event_is_event) &&
        length(single_event_is_event) == 1 &&
        !is.na(single_event_is_event)) {
      
      event_confirmed <- TRUE
      
      if (isTRUE(single_event_is_event)) {
        event_indicator <- !is.na(event_chr)
        censored_indicator <- rep(FALSE, nrow(df))
      } else {
        event_indicator <- rep(FALSE, nrow(df))
        censored_indicator <- !is.na(event_chr)
      }
    }
  }
  
  # ----------------------------------------------------------
  # 4. 유효 관측치 계산
  # ----------------------------------------------------------
  valid_rows <- !is.na(time_num) & !is.na(event_chr)
  n_valid <- sum(valid_rows)
  
  valid_time <- time_num[valid_rows]
  valid_event <- event_indicator[valid_rows]
  valid_censored <- censored_indicator[valid_rows]
  
  event_count <- if (event_confirmed) {
    sum(valid_event, na.rm = TRUE)
  } else {
    0
  }
  
  censored_count <- if (event_confirmed) {
    sum(valid_censored, na.rm = TRUE)
  } else {
    0
  }
  
  event_rate <- if (n_valid > 0) {
    event_count / n_valid
  } else {
    NA_real_
  }
  
  zero_time_ratio <- if (n_valid > 0) {
    mean(valid_time == 0)
  } else {
    NA_real_
  }
  
  # ----------------------------------------------------------
  # 5. 설명변수 처리
  # ----------------------------------------------------------
  if (is.null(x_vars) || length(x_vars) == 0) {
    x_candidates <- character()
    x_selected <- FALSE
  } else {
    x_candidates <- intersect(x_vars, names(df))
    x_candidates <- setdiff(x_candidates, c(time_var, event_var))
    x_candidates <- unique(x_candidates)
    x_selected <- length(x_candidates) > 0
  }
  
  x_detail <- make_survival_x_detail(df, type_result, x_candidates)
  modelable_x_vars <- x_detail$variable[x_detail$is_modelable]
  n_modelable_x <- length(modelable_x_vars)
  
  categorical_x_vars <- x_detail$variable[
    x_detail$is_modelable & x_detail$modelable_role == "범주형 설명변수"
  ]
  
  n_categorical_x <- length(categorical_x_vars)
  
  event_per_variable <- if (n_modelable_x > 0) {
    event_count / n_modelable_x
  } else {
    NA_real_
  }
  
  # ----------------------------------------------------------
  # 6. 필수 기준: P/NP
  # ----------------------------------------------------------
  f1 <- time_selected
  f2 <- time_selected && time_valid
  f3 <- event_selected
  f4 <- event_selected &&
    event_unique_count %in% c(1, 2) &&
    event_confirmed
  f5 <- n_valid >= 10
  f6 <- time_unique_count >= 2
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3", "F4", "F5", "F6"),
    description = c(
      "생존 시간 변수가 1개 선택되어 있음",
      "선택한 생존 시간 변수가 결측 제외 후 0 이상의 수치형 기간 변수로 해석 가능함",
      "사건 발생 여부 변수가 1개 선택되어 있음",
      "사건 발생 여부 변수의 결측 제외 고유값이 1개 또는 2개이며, 사건 발생값을 선택하거나 사건 발생 여부를 확인할 수 있음",
      "생존 시간 변수와 사건 발생 여부 변수 기준, 결측 제외 유효 관측치가 10개 이상 존재",
      "생존 시간 변수의 결측 제외 고유값 수가 2개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3, f4, f5, f6), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3, f4, f5, f6))
  
  # ----------------------------------------------------------
  # 7. 추가 안내 및 경고 문구
  # 같은 평가축에서 단계적으로 겹치는 경고는 가장 심한 문구 하나만 출력
  # 서로 다른 원인/평가축의 독립 경고는 모두 출력
  # ----------------------------------------------------------
  
  # 사건 발생 케이스 수 관련 문구
  if (event_confirmed && event_count < 5) {
    messages <- c(
      messages,
      "사건 발생 케이스가 매우 적어 생존곡선이나 생존 회귀모형 결과가 불안정할 수 있습니다."
    )
  }
  
  # 사건 발생 비율 관련 문구
  if (event_confirmed && !is.na(event_rate)) {
    if (event_rate < 0.05) {
      messages <- c(
        messages,
        "사건 발생이 매우 드물어 관련 요인 분석의 신뢰성이 낮을 수 있습니다."
      )
    } else if (event_rate > 0.95) {
      messages <- c(
        messages,
        "대부분의 관측치에서 사건이 발생하여 중도절단 정보를 활용하는 생존 분석의 장점이 제한될 수 있습니다."
      )
    }
  }
  
  if (!is.na(time_missing_rate) &&
      !is.na(event_missing_rate) &&
      (time_missing_rate >= 0.10 || event_missing_rate >= 0.10)) {
    messages <- c(
      messages,
      "생존 분석에 필요한 핵심 변수에 결측이 많아 분석 가능한 관측치가 줄어들 수 있습니다."
    )
  }
  
  if (!is.na(zero_time_ratio) && zero_time_ratio >= 0.10) {
    messages <- c(
      messages,
      "사건 발생 시간이 0으로 기록된 관측치가 많아 데이터 입력 방식이나 사건 정의를 확인할 필요가 있습니다."
    )
  }
  
  if (!x_selected) {
    messages <- c(
      messages,
      "생존곡선 분석은 가능하지만, 관련 요인 분석이나 Cox 모형 적용은 제한됩니다."
    )
  } else if (x_selected && n_modelable_x >= 1) {
    messages <- c(
      messages,
      "선택한 설명변수를 이용해 Cox 모형 또는 집단별 생존 차이 분석을 추가로 고려할 수 있습니다."
    )
  } else if (x_selected && n_modelable_x == 0) {
    messages <- c(
      messages,
      "선택한 설명변수가 분석에 적합하지 않아 관련 요인 분석은 제한됩니다."
    )
  }
  
  if (n_categorical_x >= 1) {
    messages <- c(
      messages,
      "생존 회귀모형 적용 전 범주형 설명변수의 기준 범주 설정 또는 더미변수화가 필요할 수 있습니다."
    )
  }
  
  if (x_selected &&
      n_modelable_x >= 1 &&
      !is.na(event_per_variable) &&
      event_per_variable < 5) {
    messages <- c(
      messages,
      "사건 발생 케이스 수에 비해 설명변수가 많아 Cox 모형의 추정 결과가 불안정할 수 있습니다. 설명변수 축소나 변수 선택이 필요할 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 8. 참고 사항 문구
  # ----------------------------------------------------------
  if (event_confirmed && censored_count == 0 && event_count > 0) {
    notes <- c(
      notes,
      "모든 유효 관측치에서 사건이 발생한 것으로 기록되어 중도절단 정보를 활용하는 생존 분석의 장점이 제한될 수 있습니다."
    )
  }
  
  if (event_confirmed && event_count == 0) {
    notes <- c(
      notes,
      "사건 발생 케이스가 없어 생존곡선 변화나 사건 발생 위험을 분석하기 어렵습니다."
    )
  }
  
  if (x_selected) {
    notes <- c(
      notes,
      "선택한 설명변수는 관련 요인 분석, 집단별 생존 차이 분석, Cox 모형 등에 추가로 활용될 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준 미충족 시 반환
  # ----------------------------------------------------------
  if (!essential_pass) {
    
    return(make_survival_result(
      analysis_name = "생존 분석",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      notes = notes,
      detail = list(
        essential = essential_detail,
        score_detail = NULL,
        time_variable = time_var,
        time_variable_type = time_type,
        event_variable = event_var,
        event_variable_type = event_type,
        event_unique_values = event_unique_values,
        event_unique_count = event_unique_count,
        event_value = event_value,
        single_event_is_event = single_event_is_event,
        n_valid = n_valid,
        time_unique_count = time_unique_count,
        event_count = event_count,
        censored_count = censored_count,
        event_rate = event_rate,
        time_missing_rate = time_missing_rate,
        event_missing_rate = event_missing_rate,
        zero_time_ratio = zero_time_ratio,
        x_candidates = x_candidates,
        x_detail = x_detail,
        modelable_x_vars = modelable_x_vars,
        event_per_variable = event_per_variable
      )
    ))
  }
  
  if (cox_model) {
    notes <- c(
      notes,
      "Cox proportional hazards model을 적용하려면 비례위험 가정 검토가 추가로 필요할 수 있습니다."
    )
  }
  
  # ==========================================================
  # 권장 기준 점수 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수의 충분성, 30점
  # ----------------------------------------------------------
  if (n_valid >= 100) {
    r1 <- 30
    r1_reason <- "100개 이상"
  } else if (n_valid >= 50) {
    r1 <- 24
    r1_reason <- "50개 이상 100개 미만"
  } else if (n_valid >= 30) {
    r1 <- 18
    r1_reason <- "30개 이상 50개 미만"
  } else if (n_valid >= 10) {
    r1 <- 8
    r1_reason <- "10개 이상 30개 미만"
  } else {
    r1 <- 0
    r1_reason <- "10개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 사건 발생 케이스 수의 충분성, 25점
  # ----------------------------------------------------------
  if (event_count >= 30) {
    r2 <- 25
    r2_reason <- "30개 이상"
  } else if (event_count >= 15) {
    r2 <- 18
    r2_reason <- "15개 이상 30개 미만"
  } else if (event_count >= 5) {
    r2 <- 10
    r2_reason <- "5개 이상 15개 미만"
  } else if (event_count >= 1) {
    r2 <- 3
    r2_reason <- "1개 이상 5개 미만"
  } else {
    r2 <- 0
    r2_reason <- "0개"
  }
  
  # ----------------------------------------------------------
  # R3. 사건 발생 비율의 적절성, 20점
  # ----------------------------------------------------------
  if (event_count == 0) {
    r3 <- 0
    r3_reason <- "사건 발생 케이스 0개"
  } else if (event_rate >= 0.20 && event_rate <= 0.80) {
    r3 <- 20
    r3_reason <- "20% 이상 80% 이하"
  } else if ((event_rate >= 0.10 && event_rate < 0.20) ||
             (event_rate > 0.80 && event_rate <= 0.90)) {
    r3 <- 14
    r3_reason <- "10% 이상 20% 미만 또는 80% 초과 90% 이하"
  } else if ((event_rate >= 0.05 && event_rate < 0.10) ||
             (event_rate > 0.90 && event_rate <= 0.95)) {
    r3 <- 7
    r3_reason <- "5% 이상 10% 미만 또는 90% 초과 95% 이하"
  } else {
    r3 <- 2
    r3_reason <- "5% 미만 또는 95% 초과"
  }
  
  # ----------------------------------------------------------
  # R4. 생존 시간 변수의 정보량, 15점
  # ----------------------------------------------------------
  if (time_unique_count >= 30) {
    r4 <- 15
    r4_reason <- "30개 이상"
  } else if (time_unique_count >= 15) {
    r4 <- 11
    r4_reason <- "15개 이상 30개 미만"
  } else if (time_unique_count >= 5) {
    r4 <- 6
    r4_reason <- "5개 이상 15개 미만"
  } else if (time_unique_count >= 2) {
    r4 <- 3
    r4_reason <- "2개 이상 5개 미만"
  } else {
    r4 <- 0
    r4_reason <- "2개 미만"
  }
  
  # ----------------------------------------------------------
  # R5. 결측률 및 0 시간값 영향, 10점
  # ----------------------------------------------------------
  if (time_missing_rate < 0.05 &&
      event_missing_rate < 0.05 &&
      zero_time_ratio < 0.10) {
    r5 <- 10
    r5_reason <- "생존 시간 변수 결측률 5% 미만 + 사건 변수 결측률 5% 미만 + 생존 시간값 0 비율 10% 미만"
  } else if (time_missing_rate < 0.10 &&
             event_missing_rate < 0.10 &&
             zero_time_ratio < 0.20) {
    r5 <- 7
    r5_reason <- "생존 시간 변수 결측률 10% 미만 + 사건 변수 결측률 10% 미만 + 생존 시간값 0 비율 20% 미만"
  } else if (time_missing_rate < 0.20 &&
             event_missing_rate < 0.20 &&
             zero_time_ratio < 0.30) {
    r5 <- 3
    r5_reason <- "생존 시간 변수 결측률 20% 미만 + 사건 변수 결측률 20% 미만 + 생존 시간값 0 비율 30% 미만"
  } else {
    r5 <- 0
    r5_reason <- "위 조건을 만족하지 않음"
  }
  
  # ----------------------------------------------------------
  # 총점 및 최종 판정
  # ----------------------------------------------------------
  total_score <- r1 + r2 + r3 + r4 + r5
  
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
    criterion = c("R1", "R2", "R3", "R4", "R5"),
    description = c(
      "유효 표본 수의 충분성",
      "사건 발생 케이스 수의 충분성",
      "사건 발생 비율의 적절성",
      "생존 시간 변수의 정보량",
      "결측률 및 0 생존 시간값 영향"
    ),
    score = c(r1, r2, r3, r4, r5),
    max_score = c(30, 25, 20, 15, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason)
  )
  
  # ----------------------------------------------------------
  # 요약표
  # ----------------------------------------------------------
  survival_summary <- data.frame(
    item = c(
      "생존 시간 변수",
      "사건 발생 여부 변수",
      "유효 관측치 수",
      "생존 시간 변수 고유값 수",
      "사건 발생 케이스 수",
      "중도절단 케이스 수",
      "사건 발생 비율",
      "생존 시간 변수 결측률",
      "사건 변수 결측률",
      "생존 시간값 0 비율",
      "설명변수 후보 수",
      "사용 가능한 설명변수 수",
      "사건 발생 케이스 수 / 사용 가능한 설명변수 수"
    ),
    value = c(
      time_var,
      event_var,
      n_valid,
      time_unique_count,
      event_count,
      censored_count,
      round(event_rate, 4),
      round(time_missing_rate, 4),
      round(event_missing_rate, 4),
      round(zero_time_ratio, 4),
      length(x_candidates),
      n_modelable_x,
      ifelse(is.na(event_per_variable), NA, round(event_per_variable, 4))
    )
  )
  
  # ----------------------------------------------------------
  # 최종 결과 반환
  # ----------------------------------------------------------
  make_survival_result(
    analysis_name = "생존 분석",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      survival_summary = survival_summary,
      time_variable = time_var,
      time_variable_type = time_type,
      event_variable = event_var,
      event_variable_type = event_type,
      event_unique_values = event_unique_values,
      event_unique_count = event_unique_count,
      event_value = event_value,
      single_event_is_event = single_event_is_event,
      n_valid = n_valid,
      time_unique_count = time_unique_count,
      event_count = event_count,
      censored_count = censored_count,
      event_rate = event_rate,
      time_missing_rate = time_missing_rate,
      event_missing_rate = event_missing_rate,
      zero_time_ratio = zero_time_ratio,
      x_candidates = x_candidates,
      x_detail = x_detail,
      modelable_x_vars = modelable_x_vars,
      categorical_x_vars = categorical_x_vars,
      event_per_variable = event_per_variable
    )
  )
}