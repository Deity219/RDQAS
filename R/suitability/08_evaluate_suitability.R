# ============================================================
# 08_evaluate_suitability.R
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
make_unselected_result <- function(message = "분석 목적이 선택되지 않았습니다.",
                                   selected_vars = list(),
                                   purpose = NA_character_,
                                   purpose_label = NA_character_) {
  list(
    analysis = purpose_label,
    purpose = purpose,
    purpose_label = purpose_label,
    status = "미선택",
    color = "secondary",
    score = NA_real_,
    message = message,
    recommendations = character(),
    messages = character(),
    notes = character(),
    selected_vars = selected_vars,
    detail = NULL,
    raw_result = NULL
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
convert_result_to_report <- function(res,
                                     purpose,
                                     purpose_label,
                                     selected_vars = list()) {
  
  status <- extract_status(res)
  color <- status_to_color(status)
  score <- res$score %||% NA_real_
  
  score_text <- ""
  if (!is.null(score) && length(score) == 1 && !is.na(score)) {
    score_text <- paste0(" (", round(score, 1), "점)")
  }
  
  messages <- res$messages %||% character()
  notes <- res$notes %||% character()
  recommendations <- unique(c(messages, notes))
  
  list(
    analysis = res$analysis %||% purpose_label,
    purpose = purpose,
    purpose_label = purpose_label,
    status = status,
    color = color,
    score = score,
    message = paste0(purpose_label, " 적합성 평가 결과: ", status, score_text),
    recommendations = recommendations,
    messages = messages,
    notes = notes,
    selected_vars = selected_vars,
    detail = res$detail %||% NULL,
    raw_result = res
  )
}


# ------------------------------------------------------------
# 시계열 적합성 평가 결과를 보고서용 구조로 변환
# ------------------------------------------------------------
convert_timeseries_to_report <- function(res,
                                         purpose = "time_series",
                                         purpose_label = "시계열분석",
                                         selected_vars = list()) {
  
  variable_results <- res$variable_results %||% list()
  
  if (length(variable_results) == 0) {
    return(list(
      analysis = res$analysis %||% purpose_label,
      purpose = purpose,
      purpose_label = purpose_label,
      status = "부적합",
      color = "danger",
      score = 0,
      message = paste0(purpose_label, " 적합성 평가 결과: 부적합"),
      recommendations = c("시계열 분석에 사용할 수 있는 분석 변수가 없습니다."),
      messages = c("시계열 분석에 사용할 수 있는 분석 변수가 없습니다."),
      notes = character(),
      selected_vars = selected_vars,
      detail = res$detail %||% NULL,
      variable_results = variable_results,
      results_table = res$results_table %||% NULL,
      raw_result = res
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
  
  score <- if (length(valid_scores) > 0) {
    mean(valid_scores)
  } else {
    NA_real_
  }
  
  score_text <- ""
  if (!is.na(score)) {
    score_text <- paste0(" (평균 ", round(score, 1), "점)")
  }
  
  messages <- unique(unlist(
    lapply(variable_results, function(x) {
      x$messages %||% character()
    }),
    use.names = FALSE
  ))
  
  notes <- unique(unlist(
    lapply(variable_results, function(x) {
      x$notes %||% character()
    }),
    use.names = FALSE
  ))
  
  recommendations <- unique(c(messages, notes))
  
  list(
    analysis = res$analysis %||% purpose_label,
    purpose = purpose,
    purpose_label = purpose_label,
    status = status,
    color = status_to_color(status),
    score = score,
    message = paste0(purpose_label, " 적합성 평가 결과: ", status, score_text),
    recommendations = recommendations,
    messages = messages,
    notes = notes,
    selected_vars = selected_vars,
    detail = res$detail %||% NULL,
    variable_results = variable_results,
    results_table = res$results_table %||% NULL,
    raw_result = res
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
    return(make_unselected_result(
      selected_vars = selected_vars,
      purpose = purpose,
      purpose_label = NA_character_
    ))
  }
  
  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      analysis = NA_character_,
      purpose = purpose,
      purpose_label = NA_character_,
      status = "부적합",
      color = "danger",
      score = 0,
      message = "적합성 평가에 사용할 데이터가 없습니다.",
      recommendations = character(),
      messages = character(),
      notes = character(),
      selected_vars = selected_vars,
      detail = NULL,
      raw_result = NULL
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
        purpose = purpose,
        purpose_label = ifelse(purpose == "basic_stats", "기초 통계 분석", "시각화/EDA"),
        selected_vars = selected_vars
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
        purpose = purpose,
        purpose_label = "통계적 검정 / 가설 검정",
        selected_vars = selected_vars
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
        purpose = purpose,
        purpose_label = "회귀분석",
        selected_vars = selected_vars
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
        purpose = purpose,
        purpose_label = "분류분석",
        selected_vars = selected_vars
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
        purpose = purpose,
        purpose_label = "군집분석",
        selected_vars = selected_vars
      ))
    }
    
    
    if (purpose == "time_series") {
      
      time_var <- get_selected_one(
        selected_vars,
        c("time_var", "date_var", "time_variable")
      )
      
      analysis_vars <- get_selected_vars(
        selected_vars,
        c("value_var", "value_vars", "analysis_vars", "target_vars", "selected_vars", "variables")
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
        purpose = purpose,
        purpose_label = "시계열분석",
        selected_vars = selected_vars
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
        purpose = purpose,
        purpose_label = "생존분석",
        selected_vars = selected_vars
      ))
    }
    
    
    if (purpose %in% c("correlation", "dimension")) {
      
      # 차원축소는 dim_vars를, 상관분석은 corr_variables를 우선 읽는다.
      vars <- if (purpose == "dimension") {
        get_selected_vars(
          selected_vars,
          c("dim_vars", "selected_vars", "variables", "analysis_vars")
        )
      } else {
        get_selected_vars(
          selected_vars,
          c("corr_variables", "selected_vars", "variables", "analysis_vars")
        )
      }
      
      purpose_label <- ifelse(
        purpose == "correlation",
        "상관분석",
        "차원축소/다변량 구조 분석"
      )
      
      all_numeric_vars <- type_result$variable[
        type_result$variable_type == "수치형 변수"
      ]
      
      selected_numeric_vars <- if (length(vars) > 0) {
        type_result$variable[
          type_result$variable %in% vars &
            type_result$variable_type == "수치형 변수"
        ]
      } else {
        character()
      }
      
      if (length(all_numeric_vars) < 2) {
        msg <- paste0(
          "데이터에 ", purpose_label,
          "에 사용할 수 있는 수치형 변수가 2개 미만입니다. ",
          purpose_label,
          "은 최소 2개 이상의 수치형 변수가 필요합니다."
        )
        
        return(list(
          analysis = purpose_label,
          purpose = purpose,
          purpose_label = purpose_label,
          status = "부적합",
          color = "danger",
          score = 0,
          message = paste0(purpose_label, " 적합성 평가 결과: 부적합"),
          recommendations = c(msg),
          messages = c(msg),
          notes = character(),
          selected_vars = selected_vars,
          detail = list(
            numeric_vars = all_numeric_vars,
            selected_vars = vars,
            selected_numeric_vars = selected_numeric_vars
          ),
          raw_result = NULL
        ))
      }
      
      if (length(vars) > 0 && length(selected_numeric_vars) < 2) {
        msg <- paste0(
          "선택한 변수 중 ", purpose_label,
          "에 사용할 수 있는 수치형 변수가 2개 미만입니다. 수치형 변수를 2개 이상 선택하세요."
        )
        
        return(list(
          analysis = purpose_label,
          purpose = purpose,
          purpose_label = purpose_label,
          status = "부적합",
          color = "danger",
          score = 0,
          message = paste0(purpose_label, " 적합성 평가 결과: 부적합"),
          recommendations = c(msg),
          messages = c(msg),
          notes = character(),
          selected_vars = selected_vars,
          detail = list(
            numeric_vars = all_numeric_vars,
            selected_vars = vars,
            selected_numeric_vars = selected_numeric_vars
          ),
          raw_result = NULL
        ))
      }
      
      vars_for_eda <- if (length(vars) == 0) {
        all_numeric_vars
      } else {
        selected_numeric_vars
      }
      
      res <- check_eda(
        df = data,
        type_result = type_result,
        selected_vars = vars_for_eda
      )
      
      # check_eda()는 EDA 전용 문구("EDA 적합성 점수는 ...")를 notes/messages에
      # 담아 반환한다. 상관분석/차원축소 결과에 EDA 공통 문구가 섞이지 않도록
      # 목적별 문구로 정리한다.
      purpose_specific_note <- if (purpose == "correlation") {
        paste0(
          "상관분석 적합성 점수는 선택한 수치형 변수들이 상관계수 계산과 ",
          "관계 탐색에 사용할 수 있는 구조를 갖추었는지 평가한 점수입니다."
        )
      } else {
        paste0(
          "차원축소 적합성 점수는 선택한 수치형 변수들이 주성분분석 등 ",
          "다변량 구조 분석에 사용할 수 있는 구조를 갖추었는지 평가한 점수입니다."
        )
      }
      
      sanitize_eda_text <- function(txt) {
        if (is.null(txt) || length(txt) == 0) {
          return(character())
        }
        # EDA 전용 점수 설명 문구는 제거
        txt <- txt[!grepl("EDA 적합성 점수", txt)]
        # 남은 문구의 'EDA' 표현은 목적 라벨로 치환
        txt <- gsub("데이터 탐색\\(EDA\\)", purpose_label, txt)
        txt <- gsub("EDA", purpose_label, txt)
        txt
      }
      
      res$notes <- unique(c(purpose_specific_note, sanitize_eda_text(res$notes)))
      res$messages <- sanitize_eda_text(res$messages)
      res$analysis <- purpose_label
      
      return(convert_result_to_report(
        res = res,
        purpose = purpose,
        purpose_label = purpose_label,
        selected_vars = selected_vars
      ))
    }
    
    
    # --------------------------------------------------------
    # 정의되지 않은 목적
    # --------------------------------------------------------
    make_unselected_result(
      message = "선택한 분석 목적에 해당하는 적합성 평가 함수가 없습니다.",
      selected_vars = selected_vars,
      purpose = purpose,
      purpose_label = NA_character_
    )
    
  }, error = function(e) {
    list(
      analysis = NA_character_,
      purpose = purpose,
      purpose_label = NA_character_,
      status = "부적합",
      color = "danger",
      score = 0,
      message = paste0("적합성 평가 중 오류가 발생했습니다: ", conditionMessage(e)),
      recommendations = character(),
      messages = character(),
      notes = character(),
      selected_vars = selected_vars,
      detail = NULL,
      raw_result = NULL
    )
  })
  
  result
}
