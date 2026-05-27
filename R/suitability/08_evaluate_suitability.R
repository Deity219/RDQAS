# ============================================================
# 13_evaluate_suitability.R
# 분석 목적별 적합성 평가 통합 함수
# ============================================================


# ------------------------------------------------------------
# NULL 대체 연산자
# ------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# ------------------------------------------------------------
# 선택 변수 정리 함수
# ------------------------------------------------------------
clean_selected_vars <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[x != ""]
  
  unique(x)
}


# ------------------------------------------------------------
# selected_vars에서 후보 이름 중 먼저 존재하는 값 가져오기
# ------------------------------------------------------------
get_selected_value <- function(selected_vars, candidate_names, default = NULL) {
  for (nm in candidate_names) {
    if (!is.null(selected_vars[[nm]])) {
      return(selected_vars[[nm]])
    }
  }
  default
}


# ------------------------------------------------------------
# selected_vars에서 변수 벡터 가져오기
# ------------------------------------------------------------
get_selected_vars <- function(selected_vars, candidate_names) {
  value <- get_selected_value(selected_vars, candidate_names, default = NULL)
  clean_selected_vars(value)
}


# ------------------------------------------------------------
# selected_vars에서 단일 변수명 가져오기
# ------------------------------------------------------------
get_selected_one <- function(selected_vars, candidate_names) {
  value <- get_selected_vars(selected_vars, candidate_names)
  
  if (length(value) == 0) {
    return(NULL)
  }
  
  value[1]
}


# ------------------------------------------------------------
# status에 따른 Bootstrap 계열 색상 반환
# ------------------------------------------------------------
status_to_color <- function(status) {
  if (identical(status, "적합")) {
    return("success")
  }
  
  if (identical(status, "부분 적합")) {
    return("warning")
  }
  
  if (identical(status, "부적합")) {
    return("danger")
  }
  
  "secondary"
}


# ------------------------------------------------------------
# 미선택 결과 반환
# ------------------------------------------------------------
make_unselected_result <- function(message = "분석 목적이 선택되지 않았습니다.") {
  list(
    status = "미선택",
    color = "secondary",
    message = message,
    recommendations = character()
  )
}


# ------------------------------------------------------------
# 일반 적합성 평가 결과에서 최종 판정 추출
# ------------------------------------------------------------
extract_status <- function(res) {
  
  if (!is.null(res$final_decision)) {
    return(res$final_decision)
  }
  
  # EDA처럼 final_decision이 없는 구조 보완
  if (!is.null(res$pass) && isFALSE(res$pass)) {
    return("부적합")
  }
  
  if (!is.null(res$essential_pass) && isFALSE(res$essential_pass)) {
    return("부적합")
  }
  
  if (!is.null(res$score) && !is.na(res$score)) {
    if (res$score >= 80) {
      return("적합")
    } else if (res$score >= 60) {
      return("부분 적합")
    } else {
      return("부적합")
    }
  }
  
  "부적합"
}


# ------------------------------------------------------------
# 일반 적합성 평가 결과를 보고서용 구조로 변환
# ------------------------------------------------------------
convert_result_to_report <- function(res, purpose_label) {
  
  status <- extract_status(res)
  color <- status_to_color(status)
  
  score_text <- ""
  
  if (!is.null(res$score) && !is.na(res$score)) {
    score_text <- paste0(" (", round(res$score, 1), "점)")
  }
  
  recommendations <- unique(c(
    res$messages %||% character(),
    res$notes %||% character()
  ))
  
  list(
    status = status,
    color = color,
    message = paste0(purpose_label, " 적합성 평가 결과: ", status, score_text),
    recommendations = recommendations
  )
}


# ------------------------------------------------------------
# 시계열 적합성 평가 결과를 보고서용 구조로 변환
# ------------------------------------------------------------
convert_timeseries_to_report <- function(res, purpose_label = "시계열 분석") {
  
  variable_results <- res$variable_results %||% list()
  
  if (length(variable_results) == 0) {
    return(list(
      status = "부적합",
      color = "danger",
      message = paste0(purpose_label, " 적합성 평가 결과: 부적합"),
      recommendations = character()
    ))
  }
  
  statuses <- sapply(variable_results, function(x) {
    x$final_decision %||% "부적합"
  })
  
  scores <- sapply(variable_results, function(x) {
    x$score %||% NA_real_
  })
  
  if (all(statuses == "적합")) {
    status <- "적합"
  } else if (any(statuses %in% c("적합", "부분 적합"))) {
    status <- "부분 적합"
  } else {
    status <- "부적합"
  }
  
  valid_scores <- scores[!is.na(scores)]
  
  score_text <- ""
  
  if (length(valid_scores) > 0) {
    score_text <- paste0(" (평균 ", round(mean(valid_scores), 1), "점)")
  }
  
  recommendations <- unique(unlist(
    lapply(variable_results, function(x) {
      x$messages %||% character()
    }),
    use.names = FALSE
  ))
  
  list(
    status = status,
    color = status_to_color(status),
    message = paste0(purpose_label, " 적합성 평가 결과: ", status, score_text),
    recommendations = recommendations
  )
}


# ------------------------------------------------------------
# correlation / dimension 목적용 수치형 변수 2개 이상 확인
# ------------------------------------------------------------
check_numeric_two_or_more <- function(type_result, selected_vars = character()) {
  
  if (length(selected_vars) > 0) {
    target_type <- type_result[type_result$variable %in% selected_vars, , drop = FALSE]
  } else {
    target_type <- type_result
  }
  
  numeric_vars <- target_type$variable[
    target_type$variable_type == "수치형 변수"
  ]
  
  length(numeric_vars) >= 2
}


# ------------------------------------------------------------
# 분석 목적별 적합성 평가 통합 함수
# ------------------------------------------------------------
evaluate_suitability <- function(quality_report,
                                 purpose,
                                 data = NULL,
                                 selected_vars = list()) {
  
  # quality_report는 현재 통합 평가 구조에서는 직접 사용하지 않지만,
  # 함수 시그니처 유지를 위해 인자로 둔다.
  invisible(quality_report)
  
  # ----------------------------------------------------------
  # 0. 기본 입력 확인
  # ----------------------------------------------------------
  if (is.null(purpose) || length(purpose) == 0 || is.na(purpose) || purpose == "") {
    return(make_unselected_result())
  }
  
  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      status = "부적합",
      color = "danger",
      message = "적합성 평가에 사용할 데이터가 없습니다.",
      recommendations = character()
    ))
  }
  
  # ----------------------------------------------------------
  # 1. 공통 변수 유형 판정
  # ----------------------------------------------------------
  type_result <- detect_all_variable_types(data)
  
  # ----------------------------------------------------------
  # 2. 분석 목적별 함수 호출
  # ----------------------------------------------------------
  result <- tryCatch({
    
    if (purpose %in% c("basic_stats", "visualization")) {
      
      vars <- get_selected_vars(
        selected_vars,
        c("selected_vars", "variables", "eda_vars", "analysis_vars")
      )
      
      if (length(vars) == 0) {
        vars <- NULL
      }
      
      res <- check_eda(
        df = data,
        type_result = type_result,
        selected_vars = vars
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "데이터 탐색(EDA)"
      ))
    }
    
    
    if (purpose == "advanced_ml") {
      
      group_var <- get_selected_one(
        selected_vars,
        c("group_var", "group_variable")
      )
      
      res <- check_hypothesis(
        df = data,
        type_result = type_result,
        group_var = group_var
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "가설 검정"
      ))
    }
    
    
    if (purpose == "regression") {
      
      y_var <- get_selected_one(
        selected_vars,
        c("response_var", "y_var", "target_var", "dependent_var")
      )
      
      x_vars <- get_selected_vars(
        selected_vars,
        c("predictor_vars", "x_vars", "explanatory_vars", "input_vars")
      )
      
      if (length(x_vars) == 0) {
        x_vars <- NULL
      }
      
      res <- check_regression(
        df = data,
        type_result = type_result,
        y_var = y_var,
        x_vars = x_vars
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "회귀 모델"
      ))
    }
    
    
    if (purpose == "classification") {
      
      y_var <- get_selected_one(
        selected_vars,
        c("response_var", "y_var", "target_var", "class_var", "dependent_var")
      )
      
      x_vars <- get_selected_vars(
        selected_vars,
        c("predictor_vars", "x_vars", "explanatory_vars", "input_vars")
      )
      
      if (length(x_vars) == 0) {
        x_vars <- NULL
      }
      
      res <- check_classification(
        df = data,
        type_result = type_result,
        y_var = y_var,
        x_vars = x_vars
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "분류 모델"
      ))
    }
    
    
    if (purpose == "cluster") {
      
      cluster_vars <- get_selected_vars(
        selected_vars,
        c("cluster_vars", "selected_vars", "variables", "analysis_vars")
      )
      
      if (length(cluster_vars) == 0) {
        cluster_vars <- NULL
      }
      
      res <- check_clustering(
        df = data,
        type_result = type_result,
        cluster_vars = cluster_vars
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "군집분석"
      ))
    }
    
    
    if (purpose == "time_series") {
      
      time_var <- get_selected_one(
        selected_vars,
        c("time_var", "date_var", "time_variable")
      )
      
      analysis_vars <- get_selected_vars(
        selected_vars,
        c("analysis_vars", "value_vars", "target_vars", "selected_vars", "variables")
      )
      
      forecast_model <- get_selected_value(
        selected_vars,
        c("forecast_model", "is_forecast"),
        default = FALSE
      )
      
      forecast_model <- isTRUE(forecast_model)
      
      res <- check_timeseries(
        df = data,
        type_result = type_result,
        time_var = time_var,
        analysis_vars = analysis_vars,
        forecast_model = forecast_model
      )
      
      return(convert_timeseries_to_report(
        res = res,
        purpose_label = "시계열 분석"
      ))
    }
    
    
    if (purpose == "survival") {
      
      time_var <- get_selected_one(
        selected_vars,
        c("survival_time_var", "time_var", "duration_var")
      )
      
      event_var <- get_selected_one(
        selected_vars,
        c("event_var", "status_var", "event_status_var")
      )
      
      event_value <- get_selected_value(
        selected_vars,
        c("event_value"),
        default = NULL
      )
      
      single_event_is_event <- get_selected_value(
        selected_vars,
        c("single_event_is_event"),
        default = NULL
      )
      
      x_vars <- get_selected_vars(
        selected_vars,
        c("predictor_vars", "x_vars", "explanatory_vars", "covariates")
      )
      
      if (length(x_vars) == 0) {
        x_vars <- NULL
      }
      
      cox_model <- get_selected_value(
        selected_vars,
        c("cox_model", "use_cox"),
        default = FALSE
      )
      
      cox_model <- isTRUE(cox_model)
      
      res <- check_survival(
        df = data,
        type_result = type_result,
        time_var = time_var,
        event_var = event_var,
        event_value = event_value,
        single_event_is_event = single_event_is_event,
        x_vars = x_vars,
        cox_model = cox_model
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = "생존 분석"
      ))
    }
    
    
    if (purpose %in% c("correlation", "dimension")) {
      
      vars <- get_selected_vars(
        selected_vars,
        c("selected_vars", "variables", "analysis_vars")
      )
      
      if (length(vars) == 0) {
        vars_for_check <- character()
        vars_for_eda <- NULL
      } else {
        vars_for_check <- vars
        vars_for_eda <- vars
      }
      
      res <- check_eda(
        df = data,
        type_result = type_result,
        selected_vars = vars_for_eda
      )
      
      numeric_ok <- check_numeric_two_or_more(
        type_result = type_result,
        selected_vars = vars_for_check
      )
      
      if (!numeric_ok) {
        recommendations <- unique(c(
          res$messages %||% character(),
          res$notes %||% character()
        ))
        
        return(list(
          status = "부적합",
          color = "danger",
          message = "수치형 변수 2개 이상 확인 조건을 충족하지 못했습니다.",
          recommendations = recommendations
        ))
      }
      
      purpose_label <- ifelse(
        purpose == "correlation",
        "상관 분석",
        "차원축소/다변량 구조 분석"
      )
      
      return(convert_result_to_report(
        res = res,
        purpose_label = purpose_label
      ))
    }
    
    
    # --------------------------------------------------------
    # 정의되지 않은 목적
    # --------------------------------------------------------
    make_unselected_result(
      message = "선택한 분석 목적에 해당하는 적합성 평가 함수가 없습니다."
    )
    
  }, error = function(e) {
    list(
      status = "부적합",
      color = "danger",
      message = paste0("적합성 평가 중 오류가 발생했습니다: ", conditionMessage(e)),
      recommendations = character()
    )
  })
  
  result
}
