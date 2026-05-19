# ============================================================================
# 05_check_types.R
# 자료형 오류 의심 변수 검사
# ============================================================================
# 배점: 15점
# - 문자형/factor형인데 숫자로 변환 가능한 값이 80% 이상인 경우
# - 문자형/factor형인데 날짜로 변환 가능한 값이 80% 이상인 경우
# - 하나의 변수 안에 숫자 변환 가능 값과 불가능 값이 함께 존재하는 경우
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
    if (!(is.character(x) || is.factor(x))) {
      next
    }
    
    x_chr <- trimws(as.character(x))
    x_chr <- x_chr[!is.na(x_chr) & x_chr != ""]
    
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
  
  issue_variable_count <- length(unique(type_issue_table$variable))
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
