# ============================================================================
# 05_check_types.R
# 자료형 오류 의심 변수 검사
# ============================================================================
# [파일 역할]
# - 문자형/factor형 변수 중 숫자형 또는 날짜형으로 저장되어야 할 가능성이 있는
#   변수를 탐지한다.
# - 자료형·형식 및 특수값 일관성 20점 중 "자료형 오류 의심 변수 비율" 15점을 담당한다.
#
# [수정항목 반영 내역]
# - 수정항목 4) CSV 인코딩 및 한글 변수명 처리 보완
#   * 실제 인코딩 선택은 01_quality_common.R의 quality_read_csv()와
#     나중에 수정할 03_ui.R/04_server.R에서 처리한다.
#   * 이 파일에서는 한글 변수명/한글 범주값이 들어와도 as.character() 기반으로
#     안전하게 검사하도록 작성했다.
#
# - 수정항목 5) 예외 처리
#   * 전체 결측 변수, 빈 문자열만 있는 변수, 0열 데이터에서도 오류가 나지 않도록 처리한다.
# ============================================================================

# 날짜처럼 해석 가능한 값인지 값 단위로 확인하는 보조 함수
quality_date_possible_values <- function(values) {
  values <- trimws(as.character(values))
  result <- rep(FALSE, length(values))
  
  date_formats <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
    "%m/%d/%Y", "%d/%m/%Y",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y.%m.%d %H:%M:%S"
  )
  
  for (fmt in date_formats) {
    parsed <- if (grepl("%H", fmt)) {
      suppressWarnings(as.POSIXct(values, format = fmt))
    } else {
      suppressWarnings(as.Date(values, format = fmt))
    }
    result <- result | !is.na(parsed)
  }
  
  return(result)
}

check_types_quality <- function(data, variable_types = NULL) {
  n_col <- ncol(data)
  issue_rows <- list()
  
  for (j in seq_along(data)) {
    x <- data[[j]]
    var_name <- names(data)[j]
    
    # 자료형 오류 의심은 주로 문자형/factor형 변수에서 점검한다.
    # 이미 numeric/Date로 저장된 변수는 여기서 오류로 보지 않는다.
    if (!(is.character(x) || is.factor(x))) {
      next
    }
    
    x_chr <- trimws(as.character(x))
    x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
    
    # 수정항목 5 반영:
    # 전체 결측 또는 빈 문자열뿐인 변수는 자료형 오류 검사를 건너뛴다.
    if (length(x_chr) == 0) {
      next
    }
    
    # 쉼표가 포함된 숫자 문자열도 숫자로 변환 가능한지 확인한다.
    x_num_ready <- gsub(",", "", x_chr)
    numeric_possible <- !is.na(suppressWarnings(as.numeric(x_num_ready)))
    date_possible <- quality_date_possible_values(x_chr)
    
    numeric_rate <- mean(numeric_possible)
    date_rate <- mean(date_possible)
    
    issues <- character(0)
    
    if (numeric_rate >= 0.80) {
      issues <- c(issues, "숫자형으로 저장되어야 할 가능성이 있는 문자형 변수")
    }
    
    if (date_rate >= 0.80) {
      issues <- c(issues, "날짜형으로 저장되어야 할 가능성이 있는 문자형 변수")
    }
    
    if (numeric_rate > 0 && numeric_rate < 0.80) {
      issues <- c(issues, "숫자와 문자가 혼재된 형식 의심 변수")
    }
    
    # 날짜 패턴이 일부 존재하지만 변환 성공률이 낮은 경우
    date_pattern <- grepl("\\d{4}[-/.]?\\d{1,2}[-/.]?\\d{1,2}", x_chr)
    if (any(date_pattern) && date_rate > 0 && date_rate < 0.80) {
      issues <- c(issues, "날짜 형식이 혼재된 변수")
    }
    
    if (length(issues) > 0) {
      issue_rows[[length(issue_rows) + 1]] <- data.frame(
        variable = var_name,
        issue = paste(unique(issues), collapse = "; "),
        numeric_convertible_percent = quality_percent(numeric_rate),
        date_convertible_percent = quality_percent(date_rate),
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(issue_rows) == 0) {
    type_issue_table <- data.frame(
      variable = character(0),
      issue = character(0),
      numeric_convertible_percent = numeric(0),
      date_convertible_percent = numeric(0),
      stringsAsFactors = FALSE
    )
  } else {
    type_issue_table <- do.call(rbind, issue_rows)
  }
  
  issue_variable_count <- nrow(type_issue_table)
  type_issue_rate <- quality_rate(issue_variable_count, n_col)
  score <- score_type_issue(type_issue_rate)
  
  return(list(
    item = "자료형 오류 의심 변수",
    score = score,
    max_score = 15,
    type_issue_rate = type_issue_rate,
    details = list(
      issue_variable_count = issue_variable_count,
      type_issue_percent = quality_percent(type_issue_rate),
      type_issue_table = type_issue_table
    )
  ))
}
