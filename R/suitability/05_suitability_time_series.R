# ============================================================
# 5. 시계열 분석 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_timeseries_result <- function(analysis_name = "시계열 분석",
                                   overall_pass,
                                   variable_results = list(),
                                   results_table = NULL,
                                   detail = NULL) {
  list(
    analysis = analysis_name,
    overall_pass = overall_pass,
    variable_results = variable_results,
    results_table = results_table,
    detail = detail
  )
}


# ------------------------------------------------------------
# 변수 유형 가져오기 함수
# ------------------------------------------------------------
get_variable_type <- function(type_result, var_name) {
  result <- type_result$variable_type[type_result$variable == var_name]
  
  if (length(result) == 0) {
    return(NA_character_)
  }
  
  return(result[1])
}


# ------------------------------------------------------------
# 시간 변수 변환 함수
# ------------------------------------------------------------
convert_time_values <- function(x) {
  
  if (inherits(x, c("Date"))) {
    return(as.numeric(x))
  }
  
  if (inherits(x, c("POSIXct", "POSIXlt", "POSIXt"))) {
    return(as.numeric(as.POSIXct(x)))
  }
  
  if (is.character(x) || is.factor(x)) {
    
    x <- normalize_missing(x)
    x_chr <- as.character(x)
    
    date_formats <- c(
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%Y.%m.%d",
      "%Y%m%d",
      "%m/%d/%Y",
      "%d/%m/%Y",
      "%Y-%m-%d %H:%M:%S",
      "%Y/%m/%d %H:%M:%S",
      "%Y.%m.%d %H:%M:%S"
    )
    
    parsed_num <- rep(NA_real_, length(x_chr))
    
    for (fmt in date_formats) {
      not_parsed <- is.na(parsed_num) & !is.na(x_chr)
      
      if (!any(not_parsed)) {
        break
      }
      
      if (grepl("%H", fmt)) {
        parsed <- suppressWarnings(as.POSIXct(x_chr[not_parsed], format = fmt))
        parsed_num[not_parsed] <- ifelse(
          is.na(parsed),
          parsed_num[not_parsed],
          as.numeric(parsed)
        )
      } else {
        parsed <- suppressWarnings(as.Date(x_chr[not_parsed], format = fmt))
        parsed_num[not_parsed] <- ifelse(
          is.na(parsed),
          parsed_num[not_parsed],
          as.numeric(parsed)
        )
      }
    }
    
    return(parsed_num)
  }
  
  if (is.numeric(x) || is.integer(x)) {
    
    x_nonmiss <- x[!is.na(x)]
    
    if (length(x_nonmiss) > 0 &&
        all(x_nonmiss == floor(x_nonmiss)) &&
        all(grepl("^\\d{8}$", as.character(as.integer(x_nonmiss))))) {
      
      x_chr <- as.character(as.integer(x))
      parsed <- suppressWarnings(as.Date(x_chr, format = "%Y%m%d"))
      
      if (all(!is.na(parsed[!is.na(x)]))) {
        return(as.numeric(parsed))
      }
    }
    
    return(as.numeric(x))
  }
  
  return(rep(NA_real_, length(x)))
}


# ------------------------------------------------------------
# 시간 변수 인정 여부 판정 함수
# ------------------------------------------------------------
is_valid_time_variable <- function(x) {
  
  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    return(TRUE)
  }
  
  if (is_date_convertible(x)) {
    return(TRUE)
  }
  
  if (is.numeric(x) || is.integer(x)) {
    x_nonmiss <- x[!is.na(x)]
    n_unique <- length(unique(x_nonmiss))
    
    if (n_unique >= 20) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}


# ------------------------------------------------------------
# IQR 기준 이상치 비율 계산 함수
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
# 단일 분석 변수별 시계열 적합성 평가 함수
# ------------------------------------------------------------
check_one_timeseries_variable <- function(df,
                                          type_result,
                                          time_var,
                                          analysis_var,
                                          time_selected,
                                          time_valid,
                                          forecast_model = FALSE) {
  
  messages <- character()
  
  # ----------------------------------------------------------
  # 변수 정보
  # ----------------------------------------------------------
  y_type <- get_variable_type(type_result, analysis_var)
  y_raw <- df[[analysis_var]]
  y_norm <- normalize_missing(y_raw)
  
  y_is_numeric <- analysis_var %in% names(df) &&
    is.numeric(y_raw) &&
    y_type == "수치형 변수"
  
  time_raw <- if (time_selected) df[[time_var]] else rep(NA, nrow(df))
  time_norm <- normalize_missing(time_raw)
  time_values <- if (time_selected) convert_time_values(time_raw) else rep(NA_real_, nrow(df))
  
  time_missing_rate <- mean(is.na(time_norm))
  analysis_missing_rate <- mean(is.na(y_norm))
  
  y_unique <- length(unique(y_norm[!is.na(y_norm)]))
  
  # ----------------------------------------------------------
  # 유효 관측치 / 유효 시점 수 계산
  # ----------------------------------------------------------
  valid_rows <- !is.na(time_values) & !is.na(y_norm)
  n_valid_observations <- sum(valid_rows)
  
  valid_time_values <- time_values[valid_rows]
  n_valid_timepoints <- length(unique(valid_time_values))
  
  if (n_valid_observations > 0) {
    duplicate_time_ratio <- (n_valid_observations - n_valid_timepoints) / n_valid_observations
  } else {
    duplicate_time_ratio <- NA_real_
  }
  
  unique_time_sorted <- sort(unique(valid_time_values))
  
  if (length(unique_time_sorted) >= 3) {
    time_intervals <- diff(unique_time_sorted)
    interval_mean <- mean(time_intervals)
    interval_sd <- stats::sd(time_intervals)
    
    if (interval_mean == 0 || is.na(interval_mean) || is.na(interval_sd)) {
      time_interval_cv <- 0
    } else {
      time_interval_cv <- interval_sd / interval_mean
    }
  } else {
    time_interval_cv <- NA_real_
  }
  
  outlier_rate <- if (y_is_numeric) {
    calc_outlier_rate_iqr(y_raw)
  } else {
    NA_real_
  }
  
  # ----------------------------------------------------------
  # 필수 기준: P/NP
  # ----------------------------------------------------------
  f1 <- time_selected
  f2 <- time_selected && time_valid
  f3 <- !is.null(analysis_var) && !is.na(analysis_var)
  f4 <- y_is_numeric
  f5 <- n_valid_timepoints >= 20
  f6 <- y_unique >= 2
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3", "F4", "F5", "F6"),
    description = c(
      "시간 변수가 1개 선택되어 있음",
      "선택한 시간 변수가 날짜/시간형으로 해석 가능하거나 시간 순서를 나타내는 변수임",
      "분석 수치형 변수가 1개 이상 선택되어 있음",
      "선택한 분석 변수들이 모두 수치형으로 해석 가능",
      "각 분석 변수별로, 시간 변수와 분석 변수 기준 결측 제외 유효 시점 수가 20개 이상 존재",
      "각 분석 변수별로, 분석 변수의 결측 제외 고유값 수가 2개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3, f4, f5, f6), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3, f4, f5, f6))
  
  # ----------------------------------------------------------
  # 추가 안내 및 경고 문구
  # 조건이 겹치는 경우 가장 심한 문구 하나만 출력
  # ----------------------------------------------------------
  if (n_valid_timepoints >= 20 && n_valid_timepoints < 30) {
    messages <- c(
      messages,
      "시계열 분석은 가능하지만 관측 시점이 많지 않아 추세나 예측 결과가 불안정할 수 있습니다."
    )
  }
  
  if (!is.na(duplicate_time_ratio) && duplicate_time_ratio >= 0.30) {
    messages <- c(
      messages,
      "동일한 시간값이 많이 반복되어 단순 시계열 분석 전에 집계 또는 중복 처리 여부를 확인해야 합니다."
    )
  }
  
  if (!is.na(time_interval_cv) && time_interval_cv >= 0.50) {
    messages <- c(
      messages,
      "관측 시점 간 간격이 불규칙하여 일반적인 시계열 분석이나 예측 모델 적용에 제한이 있을 수 있습니다."
    )
  }
  
  if (!is.na(time_missing_rate) && time_missing_rate >= 0.05) {
    messages <- c(
      messages,
      "시간 변수에 결측이 있어 시간 순서 정렬과 추세 해석에 문제가 생길 수 있습니다."
    )
  }
  
  if (!is.na(analysis_missing_rate) && analysis_missing_rate >= 0.20) {
    messages <- c(
      messages,
      "분석 대상 수치형 변수에 결측이 많아 추세나 변화 패턴을 안정적으로 파악하기 어려울 수 있습니다."
    )
  }
  
  if (!is.na(outlier_rate) && outlier_rate > 0.20) {
    messages <- c(
      messages,
      "극단값이 많아 추세, 이동평균, 예측 결과가 왜곡될 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준 미충족 시 반환
  # ----------------------------------------------------------
  if (!essential_pass) {
    
    return(list(
      analysis_variable = analysis_var,
      essential_pass = FALSE,
      score = NA_real_,
      final_decision = "부적합",
      messages = messages,
      detail = list(
        essential = essential_detail,
        score_detail = NULL,
        time_variable = time_var,
        analysis_variable = analysis_var,
        analysis_variable_type = y_type,
        n_valid_observations = n_valid_observations,
        n_valid_timepoints = n_valid_timepoints,
        duplicate_time_ratio = duplicate_time_ratio,
        time_interval_cv = time_interval_cv,
        time_missing_rate = time_missing_rate,
        analysis_missing_rate = analysis_missing_rate,
        analysis_unique_count = y_unique,
        outlier_rate = outlier_rate
      )
    ))
  }
  
  if (forecast_model) {
    messages <- c(
      messages,
      "ARIMA 등 일부 시계열 예측 모델을 적용할 경우 정상성 검정이나 차분 여부 확인이 추가로 필요할 수 있습니다."
    )
  }
  
  # ==========================================================
  # 권장 기준 점수 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 유효 시점 수의 충분성, 35점
  # ----------------------------------------------------------
  if (n_valid_timepoints >= 100) {
    r1 <- 35
    r1_reason <- "100개 이상"
  } else if (n_valid_timepoints >= 50) {
    r1 <- 28
    r1_reason <- "50개 이상 100개 미만"
  } else if (n_valid_timepoints >= 30) {
    r1 <- 20
    r1_reason <- "30개 이상 50개 미만"
  } else if (n_valid_timepoints >= 20) {
    r1 <- 10
    r1_reason <- "20개 이상 30개 미만"
  } else {
    r1 <- 0
    r1_reason <- "20개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 시간 간격의 규칙성, 20점
  # ----------------------------------------------------------
  if (time_interval_cv < 0.10) {
    r2 <- 20
    r2_reason <- "0.1 미만"
  } else if (time_interval_cv < 0.30) {
    r2 <- 15
    r2_reason <- "0.1 이상 0.3 미만"
  } else if (time_interval_cv < 0.50) {
    r2 <- 10
    r2_reason <- "0.3 이상 0.5 미만"
  } else if (time_interval_cv < 1.00) {
    r2 <- 5
    r2_reason <- "0.5 이상 1.0 미만"
  } else {
    r2 <- 0
    r2_reason <- "1.0 이상"
  }
  
  # ----------------------------------------------------------
  # R3. 중복 시점 비율, 15점
  # ----------------------------------------------------------
  if (duplicate_time_ratio < 0.05) {
    r3 <- 15
    r3_reason <- "5% 미만"
  } else if (duplicate_time_ratio < 0.15) {
    r3 <- 10
    r3_reason <- "5% 이상 15% 미만"
  } else if (duplicate_time_ratio < 0.30) {
    r3 <- 5
    r3_reason <- "15% 이상 30% 미만"
  } else {
    r3 <- 0
    r3_reason <- "30% 이상"
  }
  
  # ----------------------------------------------------------
  # R4. 결측률 적정성, 20점
  # ----------------------------------------------------------
  if (time_missing_rate < 0.01 && analysis_missing_rate < 0.05) {
    r4 <- 20
    r4_reason <- "시간 변수 결측률 1% 미만 + 분석 변수 결측률 5% 미만"
  } else if (time_missing_rate < 0.05 && analysis_missing_rate < 0.20) {
    r4 <- 15
    r4_reason <- "시간 변수 결측률 5% 미만 + 분석 변수 결측률 20% 미만"
  } else if (time_missing_rate < 0.10 && analysis_missing_rate < 0.40) {
    r4 <- 8
    r4_reason <- "시간 변수 결측률 10% 미만 + 분석 변수 결측률 40% 미만"
  } else {
    r4 <- 0
    r4_reason <- "시간 변수 결측률 10% 이상 또는 분석 변수 결측률 40% 이상"
  }
  
  # ----------------------------------------------------------
  # R5. 이상치 영향 정도, 10점
  # ----------------------------------------------------------
  if (outlier_rate <= 0.10) {
    r5 <- 10
    r5_reason <- "10% 이하"
  } else if (outlier_rate <= 0.20) {
    r5 <- 5
    r5_reason <- "10% 초과 20% 이하"
  } else {
    r5 <- 0
    r5_reason <- "20% 초과"
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
      "유효 시점 수의 충분성",
      "시간 간격의 규칙성",
      "중복 시점 비율",
      "결측률 적정성",
      "이상치 영향 정도"
    ),
    score = c(r1, r2, r3, r4, r5),
    max_score = c(35, 20, 15, 20, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason)
  )
  
  # ----------------------------------------------------------
  # 최종 반환
  # ----------------------------------------------------------
  list(
    analysis_variable = analysis_var,
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      time_variable = time_var,
      analysis_variable = analysis_var,
      analysis_variable_type = y_type,
      n_valid_observations = n_valid_observations,
      n_valid_timepoints = n_valid_timepoints,
      duplicate_time_ratio = duplicate_time_ratio,
      time_interval_cv = time_interval_cv,
      time_missing_rate = time_missing_rate,
      analysis_missing_rate = analysis_missing_rate,
      analysis_unique_count = y_unique,
      outlier_rate = outlier_rate
    )
  )
}


# ------------------------------------------------------------
# 시계열 분석 적합성 평가 함수
# ------------------------------------------------------------
check_timeseries <- function(df,
                             type_result,
                             time_var = NULL,
                             analysis_vars = NULL,
                             forecast_model = FALSE) {
  
  # ----------------------------------------------------------
  # 1. 시간 변수 선택 여부
  # ----------------------------------------------------------
  time_selected <- !is.null(time_var) &&
    length(time_var) == 1 &&
    !is.na(time_var) &&
    time_var %in% names(df)
  
  if (time_selected) {
    time_valid <- is_valid_time_variable(df[[time_var]])
    time_type <- get_variable_type(type_result, time_var)
  } else {
    time_valid <- FALSE
    time_type <- NA_character_
  }
  
  # ----------------------------------------------------------
  # 2. 분석 변수 선택 여부
  # ----------------------------------------------------------
  if (is.null(analysis_vars)) {
    analysis_vars <- character()
  }
  
  analysis_vars <- unique(analysis_vars[analysis_vars %in% names(df)])
  
  analysis_selected <- length(analysis_vars) >= 1
  
  # ----------------------------------------------------------
  # 3. 분석 변수가 없는 경우
  # ----------------------------------------------------------
  if (!analysis_selected) {
    
    global_essential <- data.frame(
      criterion = c("F1", "F2", "F3", "F4", "F5", "F6"),
      description = c(
        "시간 변수가 1개 선택되어 있음",
        "선택한 시간 변수가 날짜/시간형으로 해석 가능하거나 시간 순서를 나타내는 변수임",
        "분석 수치형 변수가 1개 이상 선택되어 있음",
        "선택한 분석 변수들이 모두 수치형으로 해석 가능",
        "각 분석 변수별로, 시간 변수와 분석 변수 기준 결측 제외 유효 시점 수가 20개 이상 존재",
        "각 분석 변수별로, 분석 변수의 결측 제외 고유값 수가 2개 이상 존재"
      ),
      result = c(
        ifelse(time_selected, "P", "NP"),
        ifelse(time_valid, "P", "NP"),
        "NP",
        "Not evaluated",
        "Not evaluated",
        "Not evaluated"
      )
    )
    
    return(make_timeseries_result(
      analysis_name = "시계열 분석",
      overall_pass = FALSE,
      variable_results = list(),
      results_table = data.frame(),
      detail = list(
        global_essential = global_essential,
        time_variable = time_var,
        time_variable_type = time_type,
        analysis_vars = analysis_vars
      )
    ))
  }
  
  # ----------------------------------------------------------
  # 4. 분석 변수별 평가
  # ----------------------------------------------------------
  variable_results <- lapply(analysis_vars, function(v) {
    check_one_timeseries_variable(
      df = df,
      type_result = type_result,
      time_var = time_var,
      analysis_var = v,
      time_selected = time_selected,
      time_valid = time_valid,
      forecast_model = forecast_model
    )
  })
  
  names(variable_results) <- analysis_vars
  
  # ----------------------------------------------------------
  # 5. 결과 요약표
  # ----------------------------------------------------------
  results_table <- do.call(
    rbind,
    lapply(variable_results, function(res) {
      data.frame(
        analysis_variable = res$analysis_variable,
        score = res$score,
        final_decision = res$final_decision,
        messages = paste(res$messages, collapse = " / "),
        stringsAsFactors = FALSE
      )
    })
  )
  
  overall_pass <- all(sapply(variable_results, function(res) {
    isTRUE(res$essential_pass)
  }))
  
  # ----------------------------------------------------------
  # 최종 반환
  # ----------------------------------------------------------
  make_timeseries_result(
    analysis_name = "시계열 분석",
    overall_pass = overall_pass,
    variable_results = variable_results,
    results_table = results_table,
    detail = list(
      time_variable = time_var,
      time_variable_type = time_type,
      analysis_vars = analysis_vars
    )
  )
}