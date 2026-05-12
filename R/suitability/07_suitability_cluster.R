# ============================================================
# 7. 군집분석 적합성 평가 함수
# ============================================================

# ------------------------------------------------------------
# 결과 반환 함수
# ------------------------------------------------------------
make_clustering_result <- function(analysis_name = "군집분석",
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
get_clustering_variable_type <- function(type_result, var_name) {
  result <- type_result$variable_type[type_result$variable == var_name]
  
  if (length(result) == 0) {
    return(NA_character_)
  }
  
  return(result[1])
}


# ------------------------------------------------------------
# 군집분석용 날짜형 변수 판정 함수
# ------------------------------------------------------------
is_clustering_date_variable <- function(x, var_type) {
  
  if (!is.na(var_type) && var_type == "날짜형 변수") {
    return(TRUE)
  }
  
  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    return(TRUE)
  }
  
  if (is_date_convertible(x)) {
    return(TRUE)
  }
  
  return(FALSE)
}


# ------------------------------------------------------------
# 명백한 ID성 변수 판정 함수
# ------------------------------------------------------------
is_obvious_id_variable <- function(x, var_name) {
  
  name_pattern <- "(^id$|_id$|^id_|_id_|번호|식별자|코드|name|이름)"
  
  if (grepl(name_pattern, var_name, ignore.case = TRUE)) {
    return(TRUE)
  }
  
  x_norm <- normalize_missing(x)
  x_nonmiss <- x_norm[!is.na(x_norm)]
  
  n_nonmiss <- length(x_nonmiss)
  n_unique <- length(unique(x_nonmiss))
  unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
  
  if (n_nonmiss < 30 || unique_ratio < 0.9) {
    return(FALSE)
  }
  
  if (is.numeric(x) || is.integer(x)) {
    
    x_num <- as.numeric(x_nonmiss)
    
    if (!all(is.finite(x_num))) {
      return(FALSE)
    }
    
    if (!all(x_num == floor(x_num))) {
      return(FALSE)
    }
    
    x_unique_sorted <- sort(unique(x_num))
    
    if (length(x_unique_sorted) < 2) {
      return(FALSE)
    }
    
    diffs <- diff(x_unique_sorted)
    
    if (length(diffs) > 0 && mean(diffs == 1) >= 0.9) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}


# ------------------------------------------------------------
# IQR 기준 이상치 비율 계산 함수
# ------------------------------------------------------------
calc_clustering_outlier_rate_iqr <- function(x) {
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
# IQR 0 또는 분산 0 여부 판정 함수
# ------------------------------------------------------------
has_zero_iqr_or_variance <- function(x) {
  x <- normalize_missing(x)
  x <- x[!is.na(x)]
  
  if (length(x) < 2) {
    return(TRUE)
  }
  
  iqr_value <- as.numeric(IQR(x, na.rm = TRUE))
  var_value <- as.numeric(var(x, na.rm = TRUE))
  
  if (is.na(iqr_value) || is.na(var_value)) {
    return(TRUE)
  }
  
  return(iqr_value == 0 || var_value == 0)
}


# ------------------------------------------------------------
# 군집분석 적합성 평가 함수
# ------------------------------------------------------------
check_clustering <- function(df, type_result, cluster_vars = NULL) {
  
  messages <- character()
  notes <- character()
  
  notes <- c(
    notes,
    "군집분석 적합성 점수는 데이터가 관측치 간 유사도나 거리 계산에 사용할 수 있는 구조를 갖추었는지 평가한 점수입니다. 실제 군집 수가 몇 개인지 또는 군집이 잘 분리되는지를 보장하지는 않습니다.",
    "K-means, 계층적 군집, DBSCAN 등 거리 기반 방법은 변수의 단위와 스케일에 민감하므로 표준화 또는 정규화가 필요할 수 있습니다.",
    "실제 군집 수는 Elbow method, Silhouette score, Gap statistic 등 추가 기준을 통해 검토할 수 있습니다."
  )
  
  # ----------------------------------------------------------
  # 1. 군집 대상 변수 후보 설정
  # ----------------------------------------------------------
  user_selected <- !is.null(cluster_vars) && length(cluster_vars) > 0
  
  if (user_selected) {
    cluster_candidates <- intersect(cluster_vars, names(df))
  } else {
    cluster_candidates <- type_result$variable[
      type_result$variable_type == "수치형 변수"
    ]
    cluster_candidates <- intersect(cluster_candidates, names(df))
  }
  
  cluster_candidates <- unique(cluster_candidates)
  
  # ----------------------------------------------------------
  # 2. 후보 변수별 최종 군집 대상 여부 판정
  # ----------------------------------------------------------
  if (length(cluster_candidates) == 0) {
    
    candidate_detail <- data.frame(
      variable = character(),
      variable_type = character(),
      n_non_missing = integer(),
      n_unique_non_missing = integer(),
      missing_rate = numeric(),
      unique_ratio = numeric(),
      is_numeric = logical(),
      is_date = logical(),
      is_single_value = logical(),
      is_obvious_id = logical(),
      is_categorical_selected = logical(),
      is_high_unique_continuous_numeric = logical(),
      is_final_cluster_variable = logical(),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    candidate_detail <- do.call(
      rbind,
      lapply(cluster_candidates, function(v) {
        
        x_raw <- df[[v]]
        x_norm <- normalize_missing(x_raw)
        x_nonmiss <- x_norm[!is.na(x_norm)]
        
        n_nonmiss <- length(x_nonmiss)
        n_unique <- length(unique(x_nonmiss))
        missing_rate <- mean(is.na(x_norm))
        unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
        
        var_type <- get_clustering_variable_type(type_result, v)
        
        is_numeric_var <- is.numeric(x_raw) || is.integer(x_raw)
        is_date_var <- is_clustering_date_variable(x_raw, var_type)
        is_single_value <- n_unique <= 1
        is_obvious_id <- is_obvious_id_variable(x_raw, v)
        
        is_categorical_selected <- user_selected &&
          (
            (!is.na(var_type) && var_type == "범주형 변수") ||
              is.character(x_raw) ||
              is.factor(x_raw) ||
              is.logical(x_raw)
          )
        
        is_high_unique_continuous_numeric <- is_numeric_var &&
          n_nonmiss >= 30 &&
          unique_ratio >= 0.9 &&
          !is_obvious_id &&
          !is_date_var &&
          n_unique >= 2
        
        is_final_cluster_variable <- is_numeric_var &&
          n_unique >= 2 &&
          !is_single_value &&
          !is_date_var &&
          !is_obvious_id
        
        data.frame(
          variable = v,
          variable_type = var_type,
          n_non_missing = n_nonmiss,
          n_unique_non_missing = n_unique,
          missing_rate = missing_rate,
          unique_ratio = unique_ratio,
          is_numeric = is_numeric_var,
          is_date = is_date_var,
          is_single_value = is_single_value,
          is_obvious_id = is_obvious_id,
          is_categorical_selected = is_categorical_selected,
          is_high_unique_continuous_numeric = is_high_unique_continuous_numeric,
          is_final_cluster_variable = is_final_cluster_variable,
          stringsAsFactors = FALSE
        )
      })
    )
  }
  
  final_cluster_vars <- candidate_detail$variable[
    candidate_detail$is_final_cluster_variable
  ]
  
  n_final_vars <- length(final_cluster_vars)
  
  # ----------------------------------------------------------
  # 3. 유효 관측치 계산
  # 유효 관측치 = 최종 군집 대상 변수들이 모두 결측이 아닌 행
  # ----------------------------------------------------------
  if (n_final_vars > 0) {
    
    final_df <- df[, final_cluster_vars, drop = FALSE]
    
    final_df_norm <- as.data.frame(
      lapply(final_df, normalize_missing)
    )
    
    valid_rows <- complete.cases(final_df_norm)
    n_valid <- sum(valid_rows)
    
    missing_rates <- sapply(final_df_norm, function(x) {
      mean(is.na(x))
    })
    
    avg_missing_rate <- mean(missing_rates)
    
  } else {
    
    final_df <- data.frame()
    final_df_norm <- data.frame()
    valid_rows <- rep(FALSE, nrow(df))
    n_valid <- 0
    missing_rates <- numeric()
    avg_missing_rate <- NA_real_
  }
  
  # ----------------------------------------------------------
  # 4. 부가 지표 계산
  # ----------------------------------------------------------
  sample_variable_ratio <- ifelse(n_final_vars > 0, n_valid / n_final_vars, NA_real_)
  
  avg_outlier_rate <- if (n_final_vars > 0) {
    outlier_rates <- sapply(final_cluster_vars, function(v) {
      calc_clustering_outlier_rate_iqr(df[[v]])
    })
    mean(outlier_rates)
  } else {
    NA_real_
  }
  
  zero_iqr_or_variance_vars <- if (n_final_vars > 0) {
    final_cluster_vars[
      sapply(final_cluster_vars, function(v) {
        has_zero_iqr_or_variance(df[[v]])
      })
    ]
  } else {
    character()
  }
  
  strong_corr_pair_ratio <- NA_real_
  
  if (n_final_vars >= 2 && n_valid >= 2) {
    
    corr_df <- final_df_norm[valid_rows, , drop = FALSE]
    
    corr_mat <- suppressWarnings(cor(corr_df, use = "complete.obs"))
    
    if (ncol(corr_mat) >= 2) {
      corr_values <- abs(corr_mat[upper.tri(corr_mat)])
      total_pairs <- length(corr_values)
      strong_pairs <- sum(corr_values >= 0.9, na.rm = TRUE)
      
      strong_corr_pair_ratio <- ifelse(total_pairs > 0, strong_pairs / total_pairs, NA_real_)
    }
  }
  
  # ----------------------------------------------------------
  # 5. 필수 기준: P/NP
  # ----------------------------------------------------------
  f1 <- n_final_vars >= 2
  f2 <- n_final_vars >= 1 &&
    all(candidate_detail$is_numeric[candidate_detail$is_final_cluster_variable])
  f3 <- n_final_vars >= 1 &&
    all(candidate_detail$n_unique_non_missing[candidate_detail$is_final_cluster_variable] >= 2)
  f4 <- n_final_vars >= 2 && n_valid >= 20
  
  essential_detail <- data.frame(
    criterion = c("F1", "F2", "F3", "F4"),
    description = c(
      "최종 군집 대상 수치형 변수가 2개 이상 존재",
      "최종 군집 대상 변수들이 모두 수치형으로 해석 가능",
      "각 최종 군집 대상 변수의 결측 제외 고유값 수가 2개 이상 존재",
      "최종 군집 대상 변수 기준, 결측 제외 유효 관측치가 20개 이상 존재"
    ),
    result = ifelse(c(f1, f2, f3, f4), "P", "NP")
  )
  
  essential_pass <- all(c(f1, f2, f3, f4))
  
  # ----------------------------------------------------------
  # 6. 추가 안내 및 경고 문구
  # 같은 평가축에서 단계적으로 겹치는 경고는 가장 심한 문구 하나만 출력
  # 서로 다른 원인/평가축의 독립 경고는 모두 출력
  # ----------------------------------------------------------
  
  # 최종 군집 대상 변수 수 관련 문구
  if (n_final_vars < 2) {
    messages <- c(
      messages,
      "군집분석에 사용할 수치형 변수가 부족하여 관측치 간 거리나 유사도를 안정적으로 계산하기 어렵습니다."
    )
  }
  
  if (n_final_vars >= 21) {
    messages <- c(
      messages,
      "군집 대상 변수 수가 많아 거리 기반 군집분석에서 차원의 영향이 커질 수 있습니다. 변수 선택이나 차원축소를 고려하세요."
    )
  }
  
  # 유효 관측치 수 관련 문구
  if (n_final_vars >= 2 && n_valid < 20) {
    messages <- c(
      messages,
      "군집분석에 사용할 수 있는 관측치 수가 너무 적어 군집 구조를 안정적으로 파악하기 어렵습니다."
    )
  } else if (n_final_vars >= 2 && n_valid >= 20 && n_valid < 30) {
    messages <- c(
      messages,
      "군집분석은 가능하지만 표본 수가 적어 군집 결과가 불안정할 수 있습니다."
    )
  }
  
  # 표본-변수 비율 관련 문구
  if (!is.na(sample_variable_ratio) && sample_variable_ratio < 3) {
    messages <- c(
      messages,
      "유효 관측치 수에 비해 군집 대상 변수 수가 많아 군집 결과가 불안정할 수 있습니다."
    )
  }
  
  # 결측률 관련 문구
  if (!is.na(avg_missing_rate)) {
    if (avg_missing_rate >= 0.40) {
      messages <- c(
        messages,
        "결측치 비율이 높아 군집 결과의 신뢰성이 낮을 수 있습니다. 결측 처리 또는 변수 제외가 필요합니다."
      )
    } else if (avg_missing_rate >= 0.20) {
      messages <- c(
        messages,
        "군집 대상 변수에 결측이 많아 완전한 거리 계산에 사용할 수 있는 관측치가 줄어들 수 있습니다."
      )
    }
  }
  
  # 이상치 관련 문구
  if (!is.na(avg_outlier_rate)) {
    if (avg_outlier_rate > 0.20) {
      messages <- c(
        messages,
        "이상치 비율이 높아 이상치가 별도의 군집처럼 분리되거나 전체 군집 구조를 왜곡할 수 있습니다."
      )
    } else if (avg_outlier_rate > 0.10) {
      messages <- c(
        messages,
        "일부 수치형 변수에 이상치가 있어 군집 중심이나 거리 계산이 왜곡될 수 있습니다."
      )
    }
  }
  
  # 강한 상관쌍 비율 관련 문구
  if (!is.na(strong_corr_pair_ratio) && strong_corr_pair_ratio >= 0.30) {
    messages <- c(
      messages,
      "서로 매우 유사한 변수가 많아 특정 정보가 거리 계산에 반복 반영될 수 있습니다. 변수 선택이나 차원축소를 고려하세요."
    )
  }
  
  # 변동성 관련 문구
  if (length(zero_iqr_or_variance_vars) > 0) {
    messages <- c(
      messages,
      "일부 수치형 변수의 변동이 매우 작아 군집 형성에 거의 기여하지 못할 수 있습니다. 변수 제외 여부를 확인하세요."
    )
  }
  
  # 선택 변수 유형 관련 문구
  if (user_selected && any(candidate_detail$is_categorical_selected)) {
    messages <- c(
      messages,
      "기본 군집분석 점수에는 포함하지 않습니다. 범주형 변수를 함께 사용하려면 더미변수화, Gower 거리, k-modes, k-prototypes 등 별도 방법이 필요할 수 있습니다."
    )
  }
  
  if (user_selected && any(candidate_detail$is_date)) {
    messages <- c(
      messages,
      "날짜형 변수 자체는 군집 대상 변수로 사용하기 어렵습니다. 기간, 빈도, 변화량 등 수치형 파생변수로 변환한 뒤 사용하는 것이 적절합니다."
    )
  }
  
  if (user_selected && any(candidate_detail$is_obvious_id)) {
    messages <- c(
      messages,
      "ID, 이름, 번호와 같은 식별자 변수는 관측치를 구분하기 위한 값이므로 군집 형성에 사용하기 어렵습니다."
    )
  }
  
  if (user_selected && any(candidate_detail$is_single_value)) {
    messages <- c(
      messages,
      "값의 변동이 없는 변수는 관측치 간 차이를 만들지 못하므로 군집분석 대상 변수에서 제외됩니다."
    )
  }
  
  # 고유값 비율이 높지만 ID성 변수로 제외되지 않고
  # 최종 군집 대상에 포함된 연속형 수치 변수에 대한 안내
  if (any(candidate_detail$is_final_cluster_variable &
          candidate_detail$is_high_unique_continuous_numeric)) {
    messages <- c(
      messages,
      "일반적인 연속형 수치 변수는 고유값 비율이 높더라도 군집분석에 사용할 수 있습니다. 단, 식별번호나 순번처럼 관측치를 구분하기 위한 값인지 확인하세요."
    )
  }
  
  # ----------------------------------------------------------
  # 7. 참고 사항 문구
  # ----------------------------------------------------------
  if (user_selected && any(candidate_detail$is_categorical_selected)) {
    notes <- c(
      notes,
      "범주형 변수를 포함한 군집분석은 일반적인 수치형 거리 기반 군집분석과 다른 전처리 또는 거리 척도가 필요할 수 있습니다."
    )
  }
  
  if (!is.na(avg_outlier_rate) && avg_outlier_rate > 0.10) {
    notes <- c(
      notes,
      "이상치가 군집 결과에 큰 영향을 줄 수 있으므로, 이상치 원인 확인이나 Robust scaling, DBSCAN 같은 방법을 고려할 수 있습니다."
    )
  }
  
  # ----------------------------------------------------------
  # 필수 기준 미충족 시 반환
  # ----------------------------------------------------------
  if (!essential_pass) {
    
    return(make_clustering_result(
      analysis_name = "군집분석",
      essential_pass = FALSE,
      score = 0,
      final_decision = "부적합",
      messages = messages,
      notes = notes,
      detail = list(
        essential = essential_detail,
        score_detail = NULL,
        candidate_vars = cluster_candidates,
        candidate_detail = candidate_detail,
        final_cluster_vars = final_cluster_vars,
        n_final_vars = n_final_vars,
        n_valid = n_valid,
        sample_variable_ratio = sample_variable_ratio,
        avg_missing_rate = avg_missing_rate,
        avg_outlier_rate = avg_outlier_rate,
        strong_corr_pair_ratio = strong_corr_pair_ratio,
        zero_iqr_or_variance_vars = zero_iqr_or_variance_vars,
        missing_rates = missing_rates
      )
    ))
  }
  
  # ==========================================================
  # 권장 기준 점수 계산
  # ==========================================================
  
  # ----------------------------------------------------------
  # R1. 유효 표본 수의 충분성, 25점
  # ----------------------------------------------------------
  if (n_valid >= 100) {
    r1 <- 25
    r1_reason <- "100개 이상"
  } else if (n_valid >= 50) {
    r1 <- 20
    r1_reason <- "50개 이상 100개 미만"
  } else if (n_valid >= 30) {
    r1 <- 15
    r1_reason <- "30개 이상 50개 미만"
  } else if (n_valid >= 20) {
    r1 <- 8
    r1_reason <- "20개 이상 30개 미만"
  } else {
    r1 <- 0
    r1_reason <- "20개 미만"
  }
  
  # ----------------------------------------------------------
  # R2. 최종 군집 대상 변수 수의 적절성, 20점
  # ----------------------------------------------------------
  if (n_final_vars >= 3 && n_final_vars <= 10) {
    r2 <- 20
    r2_reason <- "3개 이상 10개 이하"
  } else if (n_final_vars == 2) {
    r2 <- 16
    r2_reason <- "2개"
  } else if (n_final_vars >= 11 && n_final_vars <= 20) {
    r2 <- 12
    r2_reason <- "11개 이상 20개 이하"
  } else if (n_final_vars >= 21) {
    r2 <- 6
    r2_reason <- "21개 이상"
  } else {
    r2 <- 0
    r2_reason <- "2개 미만"
  }
  
  # ----------------------------------------------------------
  # R3. 표본 수 대비 변수 수의 충분성, 20점
  # ----------------------------------------------------------
  if (sample_variable_ratio >= 10) {
    r3 <- 20
    r3_reason <- "10 이상"
  } else if (sample_variable_ratio >= 5) {
    r3 <- 15
    r3_reason <- "5 이상 10 미만"
  } else if (sample_variable_ratio >= 3) {
    r3 <- 8
    r3_reason <- "3 이상 5 미만"
  } else {
    r3 <- 2
    r3_reason <- "3 미만"
  }
  
  # ----------------------------------------------------------
  # R4. 결측률 적정성, 15점
  # ----------------------------------------------------------
  if (avg_missing_rate < 0.05) {
    r4 <- 15
    r4_reason <- "5% 미만"
  } else if (avg_missing_rate < 0.20) {
    r4 <- 11
    r4_reason <- "5% 이상 20% 미만"
  } else if (avg_missing_rate < 0.40) {
    r4 <- 5
    r4_reason <- "20% 이상 40% 미만"
  } else {
    r4 <- 0
    r4_reason <- "40% 이상"
  }
  
  # ----------------------------------------------------------
  # R5. 이상치 영향 정도, 10점
  # ----------------------------------------------------------
  if (avg_outlier_rate <= 0.10) {
    r5 <- 10
    r5_reason <- "10% 이하"
  } else if (avg_outlier_rate <= 0.20) {
    r5 <- 5
    r5_reason <- "10% 초과 20% 이하"
  } else {
    r5 <- 0
    r5_reason <- "20% 초과"
  }
  
  # ----------------------------------------------------------
  # R6 계산 전 방어 처리
  # strong_corr_pair_ratio가 NA이면 점수 계산 오류를 방지하기 위해 0으로 처리
  # ----------------------------------------------------------
  if (is.na(strong_corr_pair_ratio)) {
    strong_corr_pair_ratio <- 0
  }
  
  # ----------------------------------------------------------
  # R6. 변수 중복성 정도, 10점
  # ----------------------------------------------------------
  if (strong_corr_pair_ratio < 0.10) {
    r6 <- 10
    r6_reason <- "10% 미만"
  } else if (strong_corr_pair_ratio < 0.30) {
    r6 <- 7
    r6_reason <- "10% 이상 30% 미만"
  } else if (strong_corr_pair_ratio < 0.50) {
    r6 <- 3
    r6_reason <- "30% 이상 50% 미만"
  } else {
    r6 <- 0
    r6_reason <- "50% 이상"
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
      "최종 군집 대상 변수 수의 적절성",
      "표본 수 대비 변수 수의 충분성",
      "결측률 적정성",
      "이상치 영향 정도",
      "변수 중복성 정도"
    ),
    score = c(r1, r2, r3, r4, r5, r6),
    max_score = c(25, 20, 20, 15, 10, 10),
    reason = c(r1_reason, r2_reason, r3_reason, r4_reason, r5_reason, r6_reason)
  )
  
  # ----------------------------------------------------------
  # 요약표
  # ----------------------------------------------------------
  clustering_summary <- data.frame(
    item = c(
      "군집 대상 변수 후보 수",
      "최종 군집 대상 수치형 변수 수",
      "유효 관측치 수",
      "표본-변수 비율",
      "평균 결측률",
      "평균 이상치 비율",
      "강한 상관쌍 비율"
    ),
    value = c(
      length(cluster_candidates),
      n_final_vars,
      n_valid,
      round(sample_variable_ratio, 4),
      round(avg_missing_rate, 4),
      round(avg_outlier_rate, 4),
      round(strong_corr_pair_ratio, 4)
    )
  )
  
  # ----------------------------------------------------------
  # 최종 결과 반환
  # ----------------------------------------------------------
  make_clustering_result(
    analysis_name = "군집분석",
    essential_pass = TRUE,
    score = total_score,
    final_decision = final_decision,
    messages = messages,
    notes = notes,
    detail = list(
      essential = essential_detail,
      score_detail = score_detail,
      clustering_summary = clustering_summary,
      candidate_vars = cluster_candidates,
      candidate_detail = candidate_detail,
      final_cluster_vars = final_cluster_vars,
      n_final_vars = n_final_vars,
      n_valid = n_valid,
      sample_variable_ratio = sample_variable_ratio,
      avg_missing_rate = avg_missing_rate,
      avg_outlier_rate = avg_outlier_rate,
      strong_corr_pair_ratio = strong_corr_pair_ratio,
      zero_iqr_or_variance_vars = zero_iqr_or_variance_vars,
      missing_rates = missing_rates
    )
  )
}