# ============================================================
# 2. 가설 검정 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_hypothesis_result <- function(analysis_name = "가설 검정",
                                   case_type,
                                   essential_pass,
                                   score = 0,
                                   final_decision = "부적합",
                                   messages = character(),
                                   notes = character(),
                                   detail = NULL) {
  list(
    analysis = analysis_name,
    case_type = case_type,
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
get_hypothesis_variable_type <- function(type_result, var_name) {
  result <- type_result$variable_type[type_result$variable == var_name]
  
  if (length(result) == 0) {
    return(NA_character_)
  }
  
  return(result[1])
}


# ------------------------------------------------------------
# 변수별 검정 후보 정보 생성 함수
# ------------------------------------------------------------
make_hypothesis_candidate_detail <- function(df, type_result, exclude_vars = character()) {
  
  candidate_pool <- setdiff(names(df), exclude_vars)
  
  if (length(candidate_pool) == 0) {
    return(data.frame(
      variable = character(),
      variable_type = character(),
      n_non_missing = integer(),
      n_unique_non_missing = integer(),
      missing_rate = numeric(),
      unique_ratio = numeric(),
      is_id = logical(),
      is_numeric_candidate = logical(),
      is_categorical_candidate = logical(),
      is_test_candidate = logical(),
      is_stable_numeric = logical(),
      is_stable_categorical = logical(),
      is_stable = logical(),
      is_high_cardinality_categorical = logical(),
      stringsAsFactors = FALSE
    ))
  }
  
  do.call(
    rbind,
    lapply(candidate_pool, function(v) {
      
      x_raw <- df[[v]]
      x_norm <- normalize_missing(x_raw)
      x_nonmiss <- x_norm[!is.na(x_norm)]
      
      n_nonmiss <- length(x_nonmiss)
      n_unique <- length(unique(x_nonmiss))
      missing_rate <- mean(is.na(x_norm))
      unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
      
      var_type <- get_hypothesis_variable_type(type_result, v)
      
      is_id <- n_nonmiss >= 30 && unique_ratio >= 0.9
      
      is_numeric_candidate <- var_type == "수치형 변수" &&
        n_unique >= 2
      
      is_categorical_candidate <- var_type %in% c("범주형 변수", "숫자형 범주 변수") &&
        n_unique >= 2
      
      is_test_candidate <- var_type %in% c(
        "수치형 변수",
        "범주형 변수",
        "숫자형 범주 변수"
      )
      
      is_stable_numeric <- FALSE
      
      if (is_numeric_candidate && n_nonmiss >= 10) {
        x_num <- suppressWarnings(as.numeric(x_nonmiss))
        
        if (all(!is.na(x_num))) {
          var_value <- suppressWarnings(var(x_num, na.rm = TRUE))
          is_stable_numeric <- !is.na(var_value) && var_value != 0
        }
      }
      
      is_stable_categorical <- FALSE
      
      if (is_categorical_candidate && n_nonmiss >= 10 && n_unique <= 10) {
        category_table <- table(x_nonmiss)
        is_stable_categorical <- min(category_table) >= 3
      }
      
      is_stable <- is_stable_numeric || is_stable_categorical
      
      is_high_cardinality_categorical <- (
        is.character(x_raw) ||
          is.factor(x_raw) ||
          is.logical(x_raw) ||
          var_type == "범주형 변수"
      ) && n_unique > 10
      
      data.frame(
        variable = v,
        variable_type = var_type,
        n_non_missing = n_nonmiss,
        n_unique_non_missing = n_unique,
        missing_rate = missing_rate,
        unique_ratio = unique_ratio,
        is_id = is_id,
        is_numeric_candidate = is_numeric_candidate,
        is_categorical_candidate = is_categorical_candidate,
        is_test_candidate = is_test_candidate,
        is_stable_numeric = is_stable_numeric,
        is_stable_categorical = is_stable_categorical,
        is_stable = is_stable,
        is_high_cardinality_categorical = is_high_cardinality_categorical,
        stringsAsFactors = FALSE
      )
    })
  )
}


# ------------------------------------------------------------
# 공통 지표 계산 함수
# ------------------------------------------------------------
calculate_hypothesis_common_metrics <- function(df,
                                                candidate_detail,
                                                group_var = NULL,
                                                group_valid = FALSE) {
  
  test_candidate_vars <- candidate_detail$variable[
    candidate_detail$is_test_candidate
  ]
  
  numeric_vars <- candidate_detail$variable[
    candidate_detail$is_numeric_candidate
  ]
  
  categorical_vars <- candidate_detail$variable[
    candidate_detail$is_categorical_candidate
  ]
  
  p <- length(numeric_vars)
  q <- length(categorical_vars)
  
  if (length(test_candidate_vars) > 0) {
    
    candidate_df <- df[, test_candidate_vars, drop = FALSE]
    
    candidate_df_norm <- as.data.frame(
      lapply(candidate_df, normalize_missing)
    )
    
    if (group_valid && !is.null(group_var) && group_var %in% names(df)) {
      group_norm <- normalize_missing(df[[group_var]])
      valid_rows <- !is.na(group_norm) &
        rowSums(!is.na(candidate_df_norm)) > 0
    } else {
      valid_rows <- rowSums(!is.na(candidate_df_norm)) > 0
    }
    
    n <- sum(valid_rows)
    missing_rate <- mean(candidate_detail$missing_rate[candidate_detail$is_test_candidate])
    
  } else {
    
    valid_rows <- rep(FALSE, nrow(df))
    n <- 0
    missing_rate <- NA_real_
  }
  
  total_candidate_count <- length(test_candidate_vars)
  stable_candidate_count <- sum(candidate_detail$is_test_candidate & candidate_detail$is_stable)
  
  stable_rate <- if (total_candidate_count > 0) {
    stable_candidate_count / total_candidate_count
  } else {
    NA_real_
  }
  
  list(
    n = n,
    p = p,
    q = q,
    missing_rate = missing_rate,
    stable_rate = stable_rate,
    valid_rows = valid_rows,
    test_candidate_vars = test_candidate_vars,
    numeric_vars = numeric_vars,
    categorical_vars = categorical_vars,
    total_candidate_count = total_candidate_count,
    stable_candidate_count = stable_candidate_count
  )
}


# ------------------------------------------------------------
# 공통 안내 및 경고 문구 생성 함수
# ------------------------------------------------------------
make_hypothesis_common_messages <- function(candidate_detail, metrics) {
  
  messages <- character()
  
  n <- metrics$n
  missing_rate <- metrics$missing_rate
  stable_rate <- metrics$stable_rate
  total_candidate_count <- metrics$total_candidate_count
  
  if (total_candidate_count < 1) {
    messages <- c(
      messages,
      "가설 검정에 사용할 수 있는 수치형 또는 범주형 변수가 없어 검정 목적에 부적합합니다. 변수 형식이나 데이터 입력 상태를 확인하세요."
    )
  }
  
  if (n < 10) {
    messages <- c(
      messages,
      "검정에 사용할 수 있는 유효 관측치 수가 너무 적어 가설 검정을 수행하기 어렵습니다."
    )
  } else if (n >= 10 && n < 30) {
    messages <- c(
      messages,
      "가설 검정은 가능하지만 표본 수가 적어 검정 결과가 불안정할 수 있습니다."
    )
  }
  
  if (!is.na(missing_rate)) {
    if (missing_rate >= 0.40) {
      messages <- c(
        messages,
        "결측치 비율이 높아 검정 결과의 신뢰성이 낮을 수 있습니다. 결측 처리 또는 변수 선택이 필요합니다."
      )
    } else if (missing_rate >= 0.20) {
      messages <- c(
        messages,
        "검정 후보 변수에 결측치가 많아 실제 검정에 사용되는 표본 수가 줄어들 수 있습니다."
      )
    }
  }
  
  if (!is.na(stable_rate)) {
    if (stable_rate < 0.20) {
      messages <- c(
        messages,
        "대부분의 검정 후보 변수가 안정적이지 않아 검정 결과 해석에 큰 제한이 있을 수 있습니다."
      )
    } else if (stable_rate < 0.50) {
      messages <- c(
        messages,
        "검정 후보 변수 중 일부는 결측, 단일값, 낮은 변동성, 희소 범주 등의 문제로 검정에 안정적으로 사용하기 어렵습니다."
      )
    }
  }
  
  if (any(candidate_detail$is_id)) {
    messages <- c(
      messages,
      "일부 변수는 고유값 수가 관측치 수에 비해 많아 ID성 변수일 가능성이 있습니다. 검정 후보 변수로 적절한지 확인이 필요합니다."
    )
  }
  
  if (any(candidate_detail$is_high_cardinality_categorical)) {
    messages <- c(
      messages,
      "범주의 종류가 많아 일부 범주의 표본 수가 부족할 수 있습니다. 범주 통합이나 변수 재분류가 필요할 수 있습니다."
    )
  }
  
  messages
}


# ------------------------------------------------------------
# 최종 판정 함수
# ------------------------------------------------------------
make_hypothesis_final_decision <- function(essential_pass, score) {
  if (!essential_pass) {
    return("부적합")
  }
  
  if (score >= 80) {
    return("적합")
  } else if (score >= 60) {
    return("부분 적합")
  } else {
    return("부적합")
  }
}


# ------------------------------------------------------------
# case1. 집단 변수가 없는 경우 적합성 평가 함수
# ------------------------------------------------------------
check_hypothesis_no_group <- function(df,
                                      type_result,
                                      group_var = NULL,
                                      group_selected = FALSE,
                                      group_k = NA_integer_) {
  
  notes <- c(
    "가설 검정 적합성 점수는 데이터가 가설 검정에 사용할 수 있는 변수 구조와 표본 안정성을 갖추었는지 평가한 점수입니다. 특정 검정 하나가 반드시 타당하다는 의미는 아닙니다.",
    "가능한 검정 목록은 변수 유형과 집단 구조를 바탕으로 한 후보 목록입니다. 실제 검정 수행 시에는 정규성, 등분산성, 독립성, 대응 구조, 기대빈도 등 검정별 가정을 추가로 확인해야 합니다.",
    "점수 계산에서는 가능한 검정 구조 중 가장 높은 점수 하나만 사용합니다. 따라서 복잡한 검정을 수행하지 못한다고 해서 불필요하게 중복 감점되지 않습니다."
  )
  
  if (group_selected) {
    notes <- c(
      notes,
      "집단변수는 p, q 계산에서 제외하고, 나머지 변수들을 검정 후보 변수로 평가합니다."
    )
  }
  
  candidate_detail <- make_hypothesis_candidate_detail(
    df = df,
    type_result = type_result,
    exclude_vars = if (group_selected && !is.null(group_var)) group_var else character()
  )
  
  metrics <- calculate_hypothesis_common_metrics(
    df = df,
    candidate_detail = candidate_detail,
    group_var = NULL,
    group_valid = FALSE
  )
  
  n <- metrics$n
  p <- metrics$p
  q <- metrics$q
  missing_rate <- metrics$missing_rate
  stable_rate <- metrics$stable_rate
  
  messages <- make_hypothesis_common_messages(candidate_detail, metrics)
  
  if (!group_selected) {
    messages <- c(
      messages,
      "집단변수 없이 가능한 검정 구조를 기준으로 적합성 점수를 계산합니다."
    )
  }
  
  if (group_selected && !is.na(group_k) && group_k <= 1) {
    messages <- c(
      messages,
      "선택한 집단변수에 비교 가능한 집단이 없어 집단 기반 검정은 수행하기 어렵습니다. 집단변수 없는 검정 기준으로 평가합니다."
    )
  }
  
  possible_tests <- character()
  
  if (p >= 1) {
    possible_tests <- c(
      possible_tests,
      "일표본 t-test",
      "Wilcoxon signed-rank",
      "정규성 검정"
    )
  }
  
  if (p >= 2) {
    possible_tests <- c(
      possible_tests,
      "대응표본 t-test",
      "Wilcoxon signed-rank",
      "Pearson 상관 검정",
      "Spearman 상관 검정",
      "Kendall 상관 검정"
    )
  }
  
  if (p >= 3) {
    possible_tests <- c(
      possible_tests,
      "여러 수치형 변수 간 상관 검정",
      "Bartlett 구형성 검정",
      "KMO"
    )
  }
  
  if (q >= 1) {
    possible_tests <- c(
      possible_tests,
      "이항검정",
      "카이제곱 적합도 검정"
    )
  }
  
  if (q >= 2) {
    possible_tests <- c(
      possible_tests,
      "카이제곱 독립성 검정",
      "Fisher 정확 검정",
      "여러 범주형 변수쌍의 관계 검정"
    )
  }
  
  possible_tests <- unique(possible_tests)
  
  if (p >= 3) {
    messages <- c(
      messages,
      "여러 수치형 변수 간 관계 구조나 다변량 수치형 구조 검정 가능성이 있습니다. 단, 다변량 검정은 표본 수와 변수 간 관계가 충분해야 안정적입니다."
    )
  } else if (p >= 2) {
    messages <- c(
      messages,
      "수치형 변수 간 상관 검정이나 대응표본 차이 검정 구조가 가능합니다. 단, 실제 검정에서는 변수 간 짝지어진 관측치 구조가 타당한지 확인해야 합니다."
    )
  } else if (p == 1) {
    messages <- c(
      messages,
      "일표본 평균 검정, Wilcoxon signed-rank 검정, 정규성 검정 등 단일 수치형 변수 검정만 가능합니다. 변수 간 관계 검정이나 대응 차이 검정은 제한됩니다."
    )
  }
  
  if (q >= 2) {
    messages <- c(
      messages,
      "범주형 변수 간 독립성 검정 구조가 가능합니다. 단, 일부 범주 조합의 빈도가 너무 작으면 Fisher 정확 검정이나 범주 통합을 고려해야 합니다."
    )
  } else if (q == 1) {
    messages <- c(
      messages,
      "이항검정 또는 카이제곱 적합도 검정처럼 단일 범주형 변수에 대한 검정만 가능합니다."
    )
  }
  
  if (p == 0 && q == 0) {
    messages <- c(
      messages,
      "현재 변수 구성으로는 집단 없이 수행 가능한 가설 검정 구조가 없습니다."
    )
  }
  
  if (p >= 1 || q >= 1) {
    notes <- c(
      notes,
      "일표본 t-test, Wilcoxon signed-rank 검정, 이항검정 등은 실제 수행 단계에서 비교 기준값 또는 기대비율 설정이 추가로 필요할 수 있습니다."
    )
  }
  
  if (p >= 3) {
    notes <- c(
      notes,
      "MANOVA, Hotelling’s T², 평균벡터 검정 등은 데이터 구조상 가능성을 의미하며, 실제 수행 시 변수 수 대비 표본 수와 검정 가정 확인이 필요합니다"
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준
  # ----------------------------------------------------------
  f1 <- metrics$total_candidate_count >= 1
  f2 <- n >= 10
  f3 <- length(possible_tests) >= 1
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3"),
    description = c(
      "수치형 또는 범주형으로 해석 가능한 검정 후보 변수가 1개 이상 존재",
      "검정 후보 변수 기준, 결측 제외 유효 관측치가 10개 이상 존재",
      "집단 없이 가능한 검정 구조가 1개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3))
  
  if (!essential_pass) {
    return(make_hypothesis_result(
      case_type = "case1",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      notes = notes,
      detail = list(
        essential = essential_detail,
        score_detail = NULL,
        group_variable = group_var,
        group_selected = group_selected,
        group_k = group_k,
        candidate_detail = candidate_detail,
        n = n,
        p = p,
        q = q,
        missing_rate = missing_rate,
        stable_rate = stable_rate,
        possible_tests = possible_tests,
        test_candidate_vars = metrics$test_candidate_vars,
        numeric_vars = metrics$numeric_vars,
        categorical_vars = metrics$categorical_vars
      )
    ))
  }
  
  # ----------------------------------------------------------
  # 권장 기준 점수 계산 전 방어 처리
  # missing_rate 또는 stable_rate가 NA인 경우 점수 계산 오류를 방지
  # ----------------------------------------------------------
  if (is.na(missing_rate)) {
    missing_rate <- 1
  }
  
  if (is.na(stable_rate)) {
    stable_rate <- 0
  }
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수, 40점
  # ----------------------------------------------------------
  if (n >= 100) {
    r1 <- 40
    r1_reason <- "100개 이상"
  } else if (n >= 50) {
    r1 <- 32
    r1_reason <- "50개 이상 100개 미만"
  } else if (n >= 30) {
    r1 <- 24
    r1_reason <- "30개 이상 50개 미만"
  } else if (n >= 10) {
    r1 <- 12
    r1_reason <- "10개 이상 30개 미만"
  } else {
    r1 <- 0
    r1_reason <- "10개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 대표 검정 구조 적합성, 30점
  # 가능한 구조 중 가장 높은 점수 하나만 사용
  # ----------------------------------------------------------
  if (p >= 3) {
    r2 <- 30
    r2_reason <- "수치형 변수 3개 이상"
  } else if (p >= 2) {
    r2 <- 25
    r2_reason <- "수치형 변수 2개 이상"
  } else if (q >= 2) {
    r2 <- 25
    r2_reason <- "범주형 변수 2개 이상"
  } else if (p >= 1) {
    r2 <- 15
    r2_reason <- "수치형 변수 1개 이상"
  } else if (q >= 1) {
    r2 <- 15
    r2_reason <- "범주형 변수 1개 이상"
  } else {
    r2 <- 0
    r2_reason <- "가능한 구조 없음"
  }
  
  # ----------------------------------------------------------
  # R3. 결측률 적정성, 20점
  # ----------------------------------------------------------
  if (missing_rate < 0.05) {
    r3 <- 20
    r3_reason <- "5% 미만"
  } else if (missing_rate < 0.20) {
    r3 <- 14
    r3_reason <- "5% 이상 20% 미만"
  } else if (missing_rate < 0.40) {
    r3 <- 7
    r3_reason <- "20% 이상 40% 미만"
  } else {
    r3 <- 0
    r3_reason <- "40% 이상"
  }
  
  # ----------------------------------------------------------
  # R4. 변수 안정성, 10점
  # ----------------------------------------------------------
  if (stable_rate >= 0.80) {
    r4 <- 10
    r4_reason <- "80% 이상"
  } else if (stable_rate >= 0.50) {
    r4 <- 7
    r4_reason <- "50% 이상 80% 미만"
  } else if (stable_rate >= 0.20) {
    r4 <- 3
    r4_reason <- "20% 이상 50% 미만"
  } else {
    r4 <- 0
    r4_reason <- "20% 미만"
  }
  
  total_score <- r1 + r2 + r3 + r4
  final_decision <- make_hypothesis_final_decision(TRUE, total_score)
  
  score_detail <- data.frame(
    criterion = c("R1", "R2", "R3", "R4"),
    description = c(
      "유효 표본 수의 충분성",
      "집단 없이 수행 가능한 대표 검정 구조의 적합성",
      "결측률 적정성",
      "변수 안정성"
    ),
    score = c(r1, r2, r3, r4),
    max_score = c(40, 30, 20, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason)
  )
  
  hypothesis_summary <- data.frame(
    item = c(
      "유효 관측치 수",
      "수치형 후보 변수 수",
      "범주형 후보 변수 수",
      "평균 결측률",
      "안정 변수 비율",
      "가능한 검정 수"
    ),
    value = c(
      n,
      p,
      q,
      round(missing_rate, 4),
      round(stable_rate, 4),
      length(possible_tests)
    )
  )
  
  make_hypothesis_result(
    case_type = "case1",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      hypothesis_summary = hypothesis_summary,
      group_variable = group_var,
      group_selected = group_selected,
      group_k = group_k,
      candidate_detail = candidate_detail,
      n = n,
      p = p,
      q = q,
      missing_rate = missing_rate,
      stable_rate = stable_rate,
      possible_tests = possible_tests,
      test_candidate_vars = metrics$test_candidate_vars,
      numeric_vars = metrics$numeric_vars,
      categorical_vars = metrics$categorical_vars
    )
  )
}


# ------------------------------------------------------------
# case2. 집단 변수가 있는 경우 적합성 평가 함수
# ------------------------------------------------------------
check_hypothesis_with_group <- function(df, type_result, group_var) {
  
  notes <- c(
    "가설 검정 적합성 점수는 데이터가 가설 검정에 사용할 수 있는 변수 구조와 표본 안정성을 갖추었는지 평가한 점수입니다. 특정 검정 하나가 반드시 타당하다는 의미는 아닙니다.",
    "가능한 검정 목록은 변수 유형과 집단 구조를 바탕으로 한 후보 목록입니다. 실제 검정 수행 시에는 정규성, 등분산성, 독립성, 대응 구조, 기대빈도 등 검정별 가정을 추가로 확인해야 합니다.",
    "집단변수는 p, q 계산에서 제외하고, 나머지 변수들을 검정 후보 변수로 평가합니다.",
    "점수 계산에서는 가능한 검정 구조 중 가장 높은 점수 하나만 사용합니다. 따라서 복잡한 검정을 수행하지 못한다고 해서 불필요하게 중복 감점되지 않습니다."
  )
  
  group_norm <- normalize_missing(df[[group_var]])
  group_nonmiss <- group_norm[!is.na(group_norm)]
  
  k_all <- length(unique(group_nonmiss))
  
  candidate_detail <- make_hypothesis_candidate_detail(
    df = df,
    type_result = type_result,
    exclude_vars = group_var
  )
  
  metrics <- calculate_hypothesis_common_metrics(
    df = df,
    candidate_detail = candidate_detail,
    group_var = group_var,
    group_valid = TRUE
  )
  
  n <- metrics$n
  p <- metrics$p
  q <- metrics$q
  missing_rate <- metrics$missing_rate
  stable_rate <- metrics$stable_rate
  valid_rows <- metrics$valid_rows
  
  group_valid_values <- group_norm[valid_rows]
  group_table <- table(group_valid_values)
  k <- length(group_table)
  min_group_n <- ifelse(k > 0, min(group_table), 0)
  
  messages <- make_hypothesis_common_messages(candidate_detail, metrics)
  
  messages <- c(
    messages,
    "선택한 집단변수를 기준으로 집단 간 차이 또는 집단별 비율 차이 검정 가능성을 평가합니다."
  )
  
  if (k == 2) {
    messages <- c(
      messages,
      "두 집단 비교 구조로 판단됩니다. 수치형 변수가 있으면 t-test, Welch t-test, Mann-Whitney U 검정 등을 고려할 수 있고, 범주형 변수가 있으면 두 집단 간 비율 차이 검정을 고려할 수 있습니다."
    )
  } else if (k >= 3) {
    messages <- c(
      messages,
      "세 집단 이상 비교 구조로 판단됩니다. 수치형 변수가 있으면 ANOVA, Welch ANOVA, Kruskal-Wallis 검정 등을 고려할 수 있습니다."
    )
  }
  
  if (k >= 11) {
    messages <- c(
      messages,
      "집단 수가 많아 각 집단의 표본 수가 부족할 가능성이 있습니다. 집단 통합이나 집단 변수 재정의가 필요한지 확인하세요."
    )
  }
  
  if (min_group_n < 3) {
    messages <- c(
      messages,
      "일부 집단의 표본 수가 매우 적어 집단 간 검정 결과가 불안정할 수 있습니다."
    )
  } else if (min_group_n < 5) {
    messages <- c(
      messages,
      "집단별 표본 수가 충분하지 않아 평균 차이 검정, 비율 차이 검정, 비모수 검정의 결과 해석에 주의가 필요합니다."
    )
  }
  
  if (p == 0 && q == 0) {
    messages <- c(
      messages,
      "집단변수는 존재하지만, 집단 간 차이나 관계를 검정할 수 있는 수치형 또는 범주형 후보 변수가 없습니다."
    )
  } else {
    
    if (p == 0) {
      messages <- c(
        messages,
        "선택한 집단변수를 기준으로 수치형 평균 또는 분포 차이 검정은 어렵습니다."
      )
    }
    
    if (q == 0) {
      messages <- c(
        messages,
        "선택한 집단변수를 기준으로 범주형 비율 또는 독립성 검정은 어렵습니다."
      )
    }
    
    if (p >= 2) {
      messages <- c(
        messages,
        "다변량 집단 차이 검정 가능성이 있습니다. 다만 Hotelling’s T² 또는 MANOVA는 표본 수와 변수 수의 균형이 중요합니다."
      )
    } else if (p >= 1) {
      messages <- c(
        messages,
        "집단별 수치형 차이 검정 구조가 가능합니다. 집단 수에 따라 t-test, ANOVA 계열 또는 비모수 검정을 고려할 수 있습니다."
      )
    }
    
    if (q >= 1) {
      messages <- c(
        messages,
        "집단변수와 다른 범주형 변수 간의 독립성 검정 또는 집단별 비율 차이 검정이 가능합니다."
      )
    }
  }
  
  possible_tests <- character()
  
  if (k >= 2 && p >= 1) {
    if (k == 2) {
      possible_tests <- c(
        possible_tests,
        "독립표본 t-test",
        "Welch t-test",
        "Mann-Whitney U"
      )
    } else if (k >= 3) {
      possible_tests <- c(
        possible_tests,
        "ANOVA",
        "Welch ANOVA",
        "Kruskal-Wallis"
      )
    }
  }
  
  if (k == 2 && p >= 2) {
    possible_tests <- c(
      possible_tests,
      "Hotelling’s T²",
      "MANOVA"
    )
  }
  
  if (k >= 3 && p >= 2) {
    possible_tests <- c(
      possible_tests,
      "MANOVA"
    )
  }
  
  if (k >= 2 && p >= 3) {
    possible_tests <- c(
      possible_tests,
      "MANOVA",
      "다변량 평균/공분산 구조 검정 가능성"
    )
  }
  
  if (k >= 2 && q >= 1) {
    possible_tests <- c(
      possible_tests,
      "집단변수와 다른 범주형 변수 간 카이제곱 검정",
      "Fisher 정확 검정"
    )
  }
  
  if (k == 2 && q >= 1) {
    possible_tests <- c(
      possible_tests,
      "두 집단 간 비율 차이 검정"
    )
  }
  
  if (k >= 3 && q >= 1) {
    possible_tests <- c(
      possible_tests,
      "여러 집단 간 범주 분포 차이 검정"
    )
  }
  
  if (q >= 2) {
    possible_tests <- c(
      possible_tests,
      "범주형 변수끼리의 독립성 검정"
    )
  }
  
  possible_tests <- unique(possible_tests)
  
  if (p >= 1 || q >= 1) {
    notes <- c(
      notes,
      "일표본 t-test, Wilcoxon signed-rank 검정, 이항검정 등은 실제 수행 단계에서 비교 기준값 또는 기대비율 설정이 추가로 필요할 수 있습니다."
    )
  }
  
  if (p >= 2) {
    notes <- c(
      notes,
      "MANOVA, Hotelling’s T², 평균벡터 검정 등은 데이터 구조상 가능성을 의미하며, 실제 수행 시 변수 수 대비 표본 수와 검정 가정 확인이 필요합니다"
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준
  # ----------------------------------------------------------
  f1 <- k >= 2
  f2 <- n >= 10
  f3 <- metrics$total_candidate_count >= 1
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3"),
    description = c(
      "유효한 집단변수가 존재함",
      "집단변수와 검정 후보 변수 기준, 결측 제외 유효 관측치가 10개 이상 존재",
      "집단변수 외에 수치형 또는 범주형 검정 후보 변수가 1개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3))
  
  if (!essential_pass) {
    return(make_hypothesis_result(
      case_type = "case2",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      notes = notes,
      detail = list(
        essential = essential_detail,
        score_detail = NULL,
        group_variable = group_var,
        group_k = k,
        group_k_all = k_all,
        min_group_n = min_group_n,
        group_table = group_table,
        candidate_detail = candidate_detail,
        n = n,
        p = p,
        q = q,
        missing_rate = missing_rate,
        stable_rate = stable_rate,
        possible_tests = possible_tests,
        test_candidate_vars = metrics$test_candidate_vars,
        numeric_vars = metrics$numeric_vars,
        categorical_vars = metrics$categorical_vars
      )
    ))
  }
  
  # ----------------------------------------------------------
  # 권장 기준 점수 계산 전 방어 처리
  # missing_rate 또는 stable_rate가 NA인 경우 점수 계산 오류를 방지
  # ----------------------------------------------------------
  if (is.na(missing_rate)) {
    missing_rate <- 1
  }
  
  if (is.na(stable_rate)) {
    stable_rate <- 0
  }
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수, 30점
  # ----------------------------------------------------------
  if (n >= 100) {
    r1 <- 30
    r1_reason <- "100개 이상"
  } else if (n >= 50) {
    r1 <- 24
    r1_reason <- "50개 이상 100개 미만"
  } else if (n >= 30) {
    r1 <- 18
    r1_reason <- "30개 이상 50개 미만"
  } else if (n >= 10) {
    r1 <- 10
    r1_reason <- "10개 이상 30개 미만"
  } else {
    r1 <- 0
    r1_reason <- "10개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 집단 구조의 적절성, 30점
  # ----------------------------------------------------------
  if (k >= 2 && k <= 5 && min_group_n >= 10) {
    r2 <- 30
    r2_reason <- "집단 수 2~5개이고, 모든 집단 표본 수 10개 이상"
  } else if (k >= 2 && k <= 10 && min_group_n >= 5) {
    r2 <- 22
    r2_reason <- "집단 수 2~10개이고, 모든 집단 표본 수 5개 이상"
  } else if (k >= 2 && min_group_n >= 3) {
    r2 <- 14
    r2_reason <- "집단 수 2개 이상이고, 모든 집단 표본 수 3개 이상"
  } else if (k >= 2 && min_group_n <= 2) {
    r2 <- 5
    r2_reason <- "집단 수 2개 이상이지만, 일부 집단 표본 수 2개 이하"
  } else {
    r2 <- 0
    r2_reason <- "집단 수 1개 이하"
  }
  
  # ----------------------------------------------------------
  # R3. 집단 기반 검정 대상 변수의 충분성, 20점
  # 가능한 집단 기반 검정 중 가장 좋은 구조 하나를 기준으로 점수 부여
  # ----------------------------------------------------------
  if (p >= 2) {
    r3 <- 20
    r3_reason <- "수치형 변수 2개 이상"
  } else if (p >= 1) {
    r3 <- 15
    r3_reason <- "수치형 변수 1개 이상"
  } else if (q >= 1) {
    r3 <- 12
    r3_reason <- "범주형 변수 1개 이상"
  } else {
    r3 <- 0
    r3_reason <- "수치형/범주형 검정 후보 변수 없음"
  }
  
  # ----------------------------------------------------------
  # R4. 결측률 적정성, 10점
  # ----------------------------------------------------------
  if (missing_rate < 0.05) {
    r4 <- 10
    r4_reason <- "5% 미만"
  } else if (missing_rate < 0.20) {
    r4 <- 7
    r4_reason <- "5% 이상 20% 미만"
  } else if (missing_rate < 0.40) {
    r4 <- 3
    r4_reason <- "20% 이상 40% 미만"
  } else {
    r4 <- 0
    r4_reason <- "40% 이상"
  }
  
  # ----------------------------------------------------------
  # R5. 변수 안정성, 10점
  # ----------------------------------------------------------
  if (stable_rate >= 0.80) {
    r5 <- 10
    r5_reason <- "80% 이상"
  } else if (stable_rate >= 0.50) {
    r5 <- 7
    r5_reason <- "50% 이상 80% 미만"
  } else if (stable_rate >= 0.20) {
    r5 <- 3
    r5_reason <- "20% 이상 50% 미만"
  } else {
    r5 <- 0
    r5_reason <- "20% 미만"
  }
  
  total_score <- r1 + r2 + r3 + r4 + r5
  final_decision <- make_hypothesis_final_decision(TRUE, total_score)
  
  score_detail <- data.frame(
    criterion = c("R1", "R2", "R3", "R4", "R5"),
    description = c(
      "유효 표본 수의 충분성",
      "집단 구조의 적절성",
      "집단 기반 검정 대상 변수의 충분성",
      "결측률 적정성",
      "변수 안정성"
    ),
    score = c(r1, r2, r3, r4, r5),
    max_score = c(30, 30, 20, 10, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason)
  )
  
  hypothesis_summary <- data.frame(
    item = c(
      "유효 관측치 수",
      "집단 수",
      "가장 작은 집단의 표본 수",
      "수치형 후보 변수 수",
      "범주형 후보 변수 수",
      "평균 결측률",
      "안정 변수 비율",
      "가능한 검정 수"
    ),
    value = c(
      n,
      k,
      min_group_n,
      p,
      q,
      round(missing_rate, 4),
      round(stable_rate, 4),
      length(possible_tests)
    )
  )
  
  make_hypothesis_result(
    case_type = "case2",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      hypothesis_summary = hypothesis_summary,
      group_variable = group_var,
      group_k = k,
      group_k_all = k_all,
      min_group_n = min_group_n,
      group_table = group_table,
      candidate_detail = candidate_detail,
      n = n,
      p = p,
      q = q,
      missing_rate = missing_rate,
      stable_rate = stable_rate,
      possible_tests = possible_tests,
      test_candidate_vars = metrics$test_candidate_vars,
      numeric_vars = metrics$numeric_vars,
      categorical_vars = metrics$categorical_vars
    )
  )
}


# ------------------------------------------------------------
# 가설 검정 적합성 평가 함수
# ------------------------------------------------------------
check_hypothesis <- function(df, type_result, group_var = NULL) {
  
  group_selected <- !is.null(group_var) &&
    length(group_var) == 1 &&
    !is.na(group_var) &&
    group_var %in% names(df)
  
  if (!group_selected) {
    return(check_hypothesis_no_group(
      df = df,
      type_result = type_result,
      group_var = NULL,
      group_selected = FALSE,
      group_k = NA_integer_
    ))
  }
  
  group_norm <- normalize_missing(df[[group_var]])
  group_nonmiss <- group_norm[!is.na(group_norm)]
  group_k <- length(unique(group_nonmiss))
  
  if (group_k <= 1) {
    return(check_hypothesis_no_group(
      df = df,
      type_result = type_result,
      group_var = group_var,
      group_selected = TRUE,
      group_k = group_k
    ))
  }
  
  check_hypothesis_with_group(
    df = df,
    type_result = type_result,
    group_var = group_var
  )
}