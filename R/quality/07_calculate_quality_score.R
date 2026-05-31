# ============================================================================
# 07_calculate_quality_score.R
# 변수 구조 / 범주형 변수 / 기본 구조 검사 및 최종 점수 계산
# ============================================================================
# [파일 역할]
# - 변수 구조 품질, 범주형 변수 품질, 데이터 기본 구조 및 변수명 품질을 검사한다.
# - 앞 파일들에서 계산한 항목별 결과를 모아 최종 품질 점수와 출력 구조를 만든다.
#
# [수정항목 반영 내역]
# - 수정항목 2) 범주형 데이터 중복 행 판정 완화
#   * 중복 완화 자체는 03_check_duplicates.R에서 수행한다.
#   * 이 파일에서는 해당 결과가 item_scores와 major_issues에 과도하게 위험으로
#     표시되지 않도록 조정된 점수를 그대로 사용한다.
#
# - 수정항목 3) 컬럼별 중복률 개념 정리
#   * 최종 결과에는 column_summary를 연결할 수 있도록 설계했다.
#   * 컬럼별 상세는 중복률이 아니라 고유값 수/고유값 비율을 쓰는 구조이다.
#
# - 수정항목 5) 빈 데이터/전체 결측 변수/상수 변수/중복 변수명 예외 처리
#   * check_variable_structure_quality(): 전체 결측 변수, 상수 변수, 분산 0 변수, ID성 변수 점검
#   * check_basic_structure_quality(): 0행, 0열, 분석 가능한 변수 없음, 빈 변수명, 중복 변수명 점검
#   * 필수 구조 조건 미충족 시 final_status를 위험으로 강제할 수 있도록 force_danger를 반환한다.
#
# - 수정항목 7) quality_report 반환 구조 정리
#   * common_result, item_scores, major_issues를 생성한다.
#   * 보고서 담당자가 바로 사용할 수 있도록 최종 결과 구조를 표 형태로 정리한다.
# ============================================================================


# ----------------------------------------------------------------------------
# 1. 변수 구조 품질 검사
# ----------------------------------------------------------------------------
# 배점: 10점
# 문제 변수:
# - 제외 변수: 결측 제외 고유값 수가 1개 이하
# - 전체 결측 변수: 결측 제외 고유값 수가 0개인 변수
# - 수치형 변수인데 분산이 0인 변수
# - ID성 변수
# 날짜형 변수는 ID성 변수 감점 대상에서 제외된다.
check_variable_structure_quality <- function(data, variable_types) {
  n_col <- ncol(data)
  
  if (n_col == 0) {
    return(list(
      item = "변수 구조 품질",
      score = 0,
      max_score = 10,
      problem_rate = 0,
      details = list(
        problem_variable_count = 0,
        problem_variable_percent = 0,
        problem_table = data.frame(variable = character(0), problem = character(0), stringsAsFactors = FALSE)
      )
    ))
  }
  
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
        problem = "값이 하나뿐이거나 전체 결측인 제외 변수",
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
  
  rownames(problem_table) <- NULL
  
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
# - ID성 변수로 판정되지는 않았지만 고유값 수가 표본 수의 50% 이상
# - 범주 수가 지나치게 많음
# ID성 변수는 변수 구조 품질에서만 감점하고 여기서는 제외한다.
check_categorical_quality <- function(data, variable_types) {
  categorical_types <- c("범주형 변수", "수치형 범주 변수", "숫자형 범주 변수")
  categorical_idx <- which(variable_types$variable_type %in% categorical_types)
  
  if (length(categorical_idx) == 0) {
    return(list(
      item = "범주형 변수 품질",
      score = 10,
      max_score = 10,
      problem_rate = 0,
      skipped = TRUE,
      warning = "범주형 변수가 없어 범주형 변수 품질 검사를 수행하지 않았습니다.",
      details = list(
        categorical_variable_count = 0,
        problem_variable_count = 0,
        problem_variable_percent = 0,
        problem_table = data.frame(variable = character(0), problem = character(0), stringsAsFactors = FALSE)
      )
    ))
  }
  
  problem_rows <- list()
  
  for (j in categorical_idx) {
    x <- data[[j]]
    x_non_missing <- x[!is.na(x)]
    
    if (length(x_non_missing) == 0) {
      next
    }
    
    freq <- table(x_non_missing)
    max_category_rate <- max(freq) / sum(freq)
    unique_count <- length(freq)
    unique_rate_by_rows <- quality_rate(unique_count, nrow(data))
    
    problems <- character(0)
    
    if (max_category_rate >= 0.90) {
      problems <- c(problems, "특정 범주의 비율이 90% 이상")
    }
    
    if (unique_rate_by_rows >= 0.50) {
      problems <- c(problems, "고유값 수가 표본 수의 50% 이상")
    }
    
    if (unique_count > 10) {
      problems <- c(problems, "범주 수가 지나치게 많음")
    }
    
    if (length(problems) > 0) {
      problem_rows[[length(problem_rows) + 1]] <- data.frame(
        variable = variable_types$variable[j],
        problem = paste(unique(problems), collapse = "; "),
        max_category_percent = quality_percent(max_category_rate),
        unique_count = unique_count,
        unique_rate = round(unique_rate_by_rows, 4),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(problem_rows) == 0) {
    problem_table <- data.frame(
      variable = character(0),
      problem = character(0),
      max_category_percent = numeric(0),
      unique_count = integer(0),
      unique_rate = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    problem_table <- do.call(rbind, problem_rows)
  }
  
  problem_rate <- quality_rate(nrow(problem_table), length(categorical_idx))
  score <- score_categorical(problem_rate)
  
  return(list(
    item = "범주형 변수 품질",
    score = score,
    max_score = 10,
    problem_rate = problem_rate,
    skipped = FALSE,
    warning = NULL,
    details = list(
      categorical_variable_count = length(categorical_idx),
      problem_variable_count = nrow(problem_table),
      problem_variable_percent = quality_percent(problem_rate),
      problem_table = problem_table
    )
  ))
}


# ----------------------------------------------------------------------------
# 3. 데이터 기본 구조 및 변수명 품질 검사
# ----------------------------------------------------------------------------
# 배점: 5점
# 수정항목 5의 핵심 부분:
# - 행 수 0개, 열 수 0개, 분석 가능한 변수 0개이면 최종 판정을 위험으로 강제한다.
# - 변수명이 비어 있거나 공백뿐인 경우, 변수명이 중복된 경우를 점검한다.
check_basic_structure_quality <- function(data, variable_types) {
  n_row <- nrow(data)
  n_col <- ncol(data)
  
  analyzable_types <- c("날짜형 변수", "범주형 변수", "수치형 범주 변수", "숫자형 범주 변수", "수치형 변수")
  analyzable_variable_count <- sum(variable_types$variable_type %in% analyzable_types)
  
  variable_names <- names(data)
  empty_name <- is.na(variable_names) | trimws(variable_names) == ""
  duplicate_name <- duplicated(variable_names) | duplicated(variable_names, fromLast = TRUE)
  name_problem <- empty_name | duplicate_name
  
  name_problem_table <- data.frame(
    variable_position = which(name_problem),
    variable_name = variable_names[name_problem],
    problem = ifelse(empty_name[name_problem], "빈 변수명 또는 공백 변수명", "중복 변수명"),
    stringsAsFactors = FALSE
  )
  
  force_danger <- n_row == 0 || n_col == 0 || analyzable_variable_count == 0
  
  if (force_danger) {
    score <- 0
  } else if (nrow(name_problem_table) > 0) {
    score <- 2
  } else if (n_row >= 10 && n_col >= 2 && analyzable_variable_count >= 1) {
    score <- 5
  } else if ((n_row >= 1 && n_row <= 9) || n_col == 1) {
    score <- 3
  } else {
    score <- 0
  }
  
  return(list(
    item = "데이터 기본 구조 및 변수명 품질",
    score = score,
    max_score = 5,
    force_danger = force_danger,
    details = list(
      row_count = n_row,
      column_count = n_col,
      analyzable_variable_count = analyzable_variable_count,
      name_problem_count = nrow(name_problem_table),
      name_problem_table = name_problem_table
    )
  ))
}


# ----------------------------------------------------------------------------
# 4. 최종 점수 계산 및 출력 구조 생성
# ----------------------------------------------------------------------------
# 수정항목 7 반영:
# - report 담당자가 바로 사용할 수 있도록 다음 3개 표를 만든다.
#   1) common_result: 전체 품질 점수, 최종 판정, 공통 판정 문구
#   2) item_scores: 평가 항목별 점수, 최대 점수, 상태
#   3) major_issues: 주의/위험 항목만 모은 주요 문제 항목
calculate_quality_score <- function(missing_result,
                                    type_result,
                                    format_result,
                                    duplicate_result,
                                    outlier_result,
                                    structure_result,
                                    categorical_result,
                                    basic_result) {
  
  type_format_score <- type_result$score + format_result$score
  
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
      type_format_score,
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
  final_status <- quality_final_status(
    total_score = total_score,
    force_danger = basic_result$force_danger
  )
  final_message <- quality_final_message(final_status)
  
  common_result <- data.frame(
    항목 = c("전체 품질 점수", "최종 판정", "공통 판정 문구"),
    결과 = c(
      paste0(total_score, "점 / 100점"),
      final_status,
      final_message
    ),
    stringsAsFactors = FALSE
  )
  
  major_issues <- item_scores[item_scores$상태 %in% c("주의", "위험"), c("평가항목", "상태")]
  
  if (nrow(major_issues) > 0) {
    major_issues$설명 <- mapply(
      quality_item_message,
      major_issues$평가항목,
      major_issues$상태,
      USE.NAMES = FALSE
    )
    names(major_issues) <- c("문제항목", "상태", "설명")
    rownames(major_issues) <- NULL
  } else {
    major_issues <- data.frame(
      문제항목 = "없음",
      상태 = "양호",
      설명 = "주요 품질 문제가 발견되지 않았습니다. 기본적인 분석과 시각화를 진행할 수 있습니다.",
      stringsAsFactors = FALSE
    )
  }
  
  rownames(item_scores) <- NULL
  rownames(common_result) <- NULL
  
  return(list(
    total_score = total_score,
    final_status = final_status,
    final_message = final_message,
    common_result = common_result,
    item_scores = item_scores,
    major_issues = major_issues
  ))
}
