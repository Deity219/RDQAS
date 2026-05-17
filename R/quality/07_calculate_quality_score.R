# ============================================================================
# 07_calculate_quality_score.R
# 변수 구조 / 범주형 변수 / 기본 구조 검사 및 최종 점수 계산
# ============================================================================
# 이 파일에는 점수 계산에 필요한 마지막 항목들을 함께 둔다.
# 파일 수가 지나치게 많아지는 것을 막기 위한 구성이다.
# ============================================================================

# ----------------------------------------------------------------------------
# 1. 변수 구조 품질 검사
# ----------------------------------------------------------------------------
# 배점: 10점
# 문제 변수:
# - 제외 변수: 결측 제외 고유값 수가 1개 이하
# - 수치형 변수인데 분산이 0인 변수
# - ID성 변수
# 날짜형 변수는 ID성 변수 감점 대상에서 제외된다.
check_variable_structure_quality <- function(data, variable_types) {
  n_col <- ncol(data)
  
  exclude_idx <- which(variable_types$variable_type == "제외 변수")
  id_idx <- which(variable_types$variable_type == "ID성 변수")
  
  zero_var_idx <- integer(0)
  numeric_idx <- which(variable_types$variable_type == "수치형 변수")
  
  for (j in numeric_idx) {
    x <- data[[j]]
    x_valid <- x[!is.na(x) & is.finite(x)]
    
    if (length(x_valid) > 1) {
      var_value <- stats::var(x_valid)
      if (!is.na(var_value) && var_value == 0) {
        zero_var_idx <- c(zero_var_idx, j)
      }
    }
  }
  
  problem_idx <- sort(unique(c(exclude_idx, id_idx, zero_var_idx)))
  problem_rate <- quality_rate(length(problem_idx), n_col)
  score <- score_variable_structure(problem_rate)
  
  problem_table <- data.frame(
    variable = character(0),
    problem = character(0),
    stringsAsFactors = FALSE
  )
  
  if (length(exclude_idx) > 0) {
    problem_table <- rbind(
      problem_table,
      data.frame(
        variable = variable_types$variable[exclude_idx],
        problem = "값이 하나뿐이거나 결측 제외 고유값 수가 1개 이하인 제외 변수",
        stringsAsFactors = FALSE
      )
    )
  }
  
  if (length(id_idx) > 0) {
    problem_table <- rbind(
      problem_table,
      data.frame(
        variable = variable_types$variable[id_idx],
        problem = "고유값 비율이 높아 ID성 변수로 의심되는 변수",
        stringsAsFactors = FALSE
      )
    )
  }
  
  if (length(zero_var_idx) > 0) {
    problem_table <- rbind(
      problem_table,
      data.frame(
        variable = variable_types$variable[zero_var_idx],
        problem = "수치형 변수이지만 분산이 0인 변수",
        stringsAsFactors = FALSE
      )
    )
  }
  
  return(list(
    item = "변수 구조 품질",
    score = score,
    max_score = 10,
    problem_rate = problem_rate,
    details = list(
      problem_variable_count = length(problem_idx),
      problem_variable_percent = quality_percent(problem_rate),
      problem_table = problem_table
    )
  ))
}

# ----------------------------------------------------------------------------
# 2. 범주형 변수 품질 검사
# ----------------------------------------------------------------------------
# 배점: 10점
# 문제 변수:
# - 특정 범주의 비율이 90% 이상
# - 고유값 수가 표본 수의 50% 이상
# - 범주 수가 지나치게 많음
# ID성 변수는 변수 구조 품질에서만 감점하고 여기서는 제외한다.
check_categorical_quality <- function(data, variable_types) {
  base_categorical_idx <- which(
    variable_types$variable_type %in% c("범주형 변수", quality_numeric_category_types())
  )
  
  # 00_variable_type.R 기준에서 문자형인데 범주 수가 10개를 넘으면 "기타 변수"가 될 수 있다.
  # 이런 변수도 범주 수 과다 문제를 확인하기 위해 후보에 포함한다.
  extra_character_idx <- which(
    variable_types$variable_type == "기타 변수" &
      vapply(data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))
  )
  
  categorical_idx <- sort(unique(c(base_categorical_idx, extra_character_idx)))
  
  if (length(categorical_idx) == 0) {
    return(list(
      item = "범주형 변수 품질",
      score = 10,
      max_score = 10,
      problem_rate = 0,
      details = list(
        categorical_variable_count = 0,
        problem_categorical_count = 0,
        problem_categorical_percent = 0,
        categorical_table = data.frame(
          variable = character(0),
          n_unique = integer(0),
          max_category_percent = numeric(0),
          unique_ratio = numeric(0),
          problem = logical(0),
          issue = character(0),
          stringsAsFactors = FALSE
        )
      )
    ))
  }
  
  categorical_table <- do.call(rbind, lapply(categorical_idx, function(j) {
    x <- data[[j]]
    x_nonmiss <- x[!is.na(x)]
    n_nonmiss <- length(x_nonmiss)
    n_unique <- length(unique(x_nonmiss))
    unique_ratio <- quality_rate(n_unique, n_nonmiss)
    
    if (n_nonmiss == 0) {
      max_category_rate <- 0
    } else {
      freq <- table(x_nonmiss)
      max_category_rate <- max(freq) / n_nonmiss
    }
    
    issues <- character(0)
    
    if (max_category_rate >= 0.90) {
      issues <- c(issues, "특정 범주의 비율이 90% 이상")
    }
    
    if (unique_ratio >= 0.50) {
      issues <- c(issues, "고유값 수가 표본 수의 50% 이상")
    }
    
    if (n_unique > 10) {
      issues <- c(issues, "범주 수가 지나치게 많음")
    }
    
    data.frame(
      variable = variable_types$variable[j],
      n_unique = n_unique,
      max_category_percent = quality_percent(max_category_rate),
      unique_ratio = round(unique_ratio, 4),
      problem = length(issues) > 0,
      issue = ifelse(length(issues) > 0, paste(issues, collapse = "; "), ""),
      stringsAsFactors = FALSE
    )
  }))
  
  problem_categorical_count <- sum(categorical_table$problem)
  problem_rate <- quality_rate(problem_categorical_count, length(categorical_idx))
  score <- score_categorical(problem_rate)
  
  return(list(
    item = "범주형 변수 품질",
    score = score,
    max_score = 10,
    problem_rate = problem_rate,
    details = list(
      categorical_variable_count = length(categorical_idx),
      problem_categorical_count = problem_categorical_count,
      problem_categorical_percent = quality_percent(problem_rate),
      categorical_table = categorical_table
    )
  ))
}

# ----------------------------------------------------------------------------
# 3. 데이터 기본 구조 및 변수명 품질 검사
# ----------------------------------------------------------------------------
# 배점: 5점
# 필수 구조 조건 미충족 시 최종 판정을 위험으로 강제할 수 있다.
check_basic_structure_quality <- function(data, variable_types) {
  n_row <- nrow(data)
  n_col <- ncol(data)
  names_vec <- names(data)
  
  empty_name <- is.na(names_vec) | trimws(names_vec) == ""
  duplicated_name <- duplicated(names_vec) | duplicated(names_vec, fromLast = TRUE)
  has_name_problem <- any(empty_name | duplicated_name)
  
  analysis_variable_count <- quality_analysis_variable_count(variable_types)
  force_danger <- n_row == 0 || n_col == 0 || analysis_variable_count == 0
  
  if (force_danger) {
    score <- 0
  } else if (has_name_problem) {
    score <- 2
  } else if (n_row >= 10 && n_col >= 2 && analysis_variable_count >= 1) {
    score <- 5
  } else {
    score <- 3
  }
  
  name_problem_table <- data.frame(
    variable = names_vec[empty_name | duplicated_name],
    problem = ifelse(
      empty_name[empty_name | duplicated_name],
      "변수명이 비어 있거나 공백으로만 구성됨",
      "변수명이 중복됨"
    ),
    stringsAsFactors = FALSE
  )
  
  return(list(
    item = "데이터 기본 구조 및 변수명 품질",
    score = score,
    max_score = 5,
    force_danger = force_danger,
    details = list(
      row_count = n_row,
      column_count = n_col,
      analysis_variable_count = analysis_variable_count,
      has_name_problem = has_name_problem,
      empty_name_count = sum(empty_name),
      duplicated_name_count = sum(duplicated_name),
      name_problem_table = name_problem_table
    )
  ))
}

# ----------------------------------------------------------------------------
# 4. 최종 점수 계산
# ----------------------------------------------------------------------------
calculate_quality_score <- function(missing_result,
                                    type_result,
                                    format_result,
                                    duplicate_result,
                                    outlier_result,
                                    structure_result,
                                    categorical_result,
                                    basic_result) {
  item_scores <- data.frame(
    평가항목 = c(
      "결측치 품질",
      "자료형·형식 및 특수값 일관성",
      "중복값 품질",
      "이상치 품질",
      "변수 구조 품질",
      "범주형 변수 품질",
      "데이터 기본 구조 및 변수명 품질"
    ),
    점수 = c(
      missing_result$score,
      type_result$score + format_result$score,
      duplicate_result$score,
      outlier_result$score,
      structure_result$score,
      categorical_result$score,
      basic_result$score
    ),
    최대점수 = c(25, 20, 15, 15, 10, 10, 5),
    stringsAsFactors = FALSE
  )
  
  item_scores$상태 <- mapply(
    quality_item_status,
    item_scores$점수,
    item_scores$최대점수,
    USE.NAMES = FALSE
  )
  
  total_score <- sum(item_scores$점수)
  final_status <- quality_final_status(total_score, basic_result$force_danger)
  final_message <- quality_final_message(final_status)
  
  major_issues <- item_scores[item_scores$상태 != "양호", c("평가항목", "상태")]
  
  if (nrow(major_issues) == 0) {
    major_issues <- data.frame(
      문제항목 = "없음",
      상태 = "양호",
      설명 = "주요 품질 문제가 발견되지 않았습니다. 기본적인 분석과 시각화를 진행할 수 있습니다.",
      stringsAsFactors = FALSE
    )
  } else {
    major_issues$설명 <- mapply(
      quality_issue_message,
      major_issues$평가항목,
      major_issues$상태,
      USE.NAMES = FALSE
    )
    names(major_issues)[names(major_issues) == "평가항목"] <- "문제항목"
  }
  
  common_result <- data.frame(
    항목 = c("전체 품질 점수", "최종 판정", "공통 판정 문구"),
    결과 = c(paste0(total_score, "점 / 100점"), final_status, final_message),
    stringsAsFactors = FALSE
  )
  
  return(list(
    total_score = total_score,
    final_status = final_status,
    final_message = final_message,
    common_result = common_result,
    item_scores = item_scores,
    major_issues = major_issues
  ))
}
