# ============================================================
# 변수 유형 자동 판정 기준
# ============================================================

# ── 1. 결측 표현 정규화 ───────────────────────────────────────
normalize_missing <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  
  if (is.character(x)) {
    x <- trimws(x)
    x[x %in% c("", "NA", "N/A", "na", "n/a",
               "null", "NULL", ".", "NaN", "-")] <- NA
  }
  
  return(x)
}


# ── 2. 날짜 판정용 값 제한 함수: 전체 고유값에서 균등하게 확인 ─────
limit_check_values <- function(x, max_n = 1000) {
  x <- unique(x)
  n <- length(x)
  
  if (n <= max_n) {
    return(x)
  }
  
  idx <- unique(round(seq(1, n, length.out = max_n)))
  
  return(x[idx])
}


# ── 3. 날짜 변환 가능 여부 판정 ─────────────────────────────────
is_date_convertible <- function(x) {
  
  # 이미 Date/POSIX 계열이면 날짜형으로 판정
  if (inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
    return(TRUE)
  }
  
  # 문자형/factor형 날짜 처리
  if (is.character(x) || is.factor(x)) {
    x <- normalize_missing(x)
    x_nonmiss <- x[!is.na(x)]
    
    if (length(x_nonmiss) == 0) {
      return(FALSE)
    }
    
    # 결측 제외 고유값이 너무 많으면 전체 범위에서 균등하게 1000개만 확인
    x_nonmiss <- limit_check_values(x_nonmiss, max_n = 1000)
    
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
    
    parsed_success <- rep(FALSE, length(x_nonmiss))
    
    for (fmt in date_formats) {
      if (grepl("%H", fmt)) {
        parsed <- suppressWarnings(as.POSIXct(x_nonmiss, format = fmt))
      } else {
        parsed <- suppressWarnings(as.Date(x_nonmiss, format = fmt))
      }
      parsed_success <- parsed_success | !is.na(parsed)
    }
    
    parse_rate <- mean(parsed_success)
    return(parse_rate >= 0.90)
  }
  
  # numeric/integer 형태의 YYYYMMDD 날짜 처리
  if (is.numeric(x) || is.integer(x)) {
    x_nonmiss <- x[!is.na(x)]
    
    if (length(x_nonmiss) == 0) {
      return(FALSE)
    }
    
    # 결측 제외 고유값이 너무 많으면 전체 범위에서 균등하게 1000개만 확인
    x_nonmiss <- limit_check_values(x_nonmiss, max_n = 1000)
    
    # 소수점이 있는 수치형 변수는 날짜형으로 보지 않음
    if (!all(x_nonmiss == floor(x_nonmiss))) {
      return(FALSE)
    }
    
    # 모두 8자리 정수 YYYYMMDD 형태인지 확인
    x_chr <- as.character(as.integer(x_nonmiss))
    
    is_yyyymmdd <- grepl("^\\d{8}$", x_chr)
    
    if (!all(is_yyyymmdd)) {
      return(FALSE)
    }
    
    parsed <- suppressWarnings(as.Date(x_chr, format = "%Y%m%d"))
    
    return(all(!is.na(parsed)))
  }
  
  return(FALSE)
}


# ── 4. 단일 변수 유형 자동 판정 ─────────────────────────────────
detect_variable_type <- function(x) {
  
  x_for_count <- normalize_missing(x)
  x_nonmiss <- x_for_count[!is.na(x_for_count)]
  
  n_nonmiss <- length(x_nonmiss)
  n_unique <- length(unique(x_nonmiss))
  
  unique_ratio <- ifelse(n_nonmiss == 0, 0, n_unique / n_nonmiss)
  
  # 1. 제외 변수
  # 결측 제외 고유값 수가 1개 이하
  if (n_unique <= 1) {
    return("제외 변수")
  }
  
  # 2. 날짜형 변수
  # Date/POSIXct이거나 날짜로 변환 가능한 변수
  # 날짜형은 ID성 변수보다 먼저 판정해야 함
  if (is_date_convertible(x)) {
    return("날짜형 변수")
  }
  
  # 3. ID성 변수
  # 결측 제외 관측치 수가 30개 이상이고,
  # 고유값 수 / 결측 제외 관측치 수 >= 0.9
  if (n_nonmiss >= 30 && unique_ratio >= 0.9) {
    return("ID성 변수")
  }
  
  # 4. 범주형 변수
  # 문자형, factor형, logical형 변수 중 고유값 수가 2개 이상 10개 이하
  if ((is.character(x) || is.factor(x) || is.logical(x)) &&
      n_unique >= 2 && n_unique <= 10) {
    return("범주형 변수")
  }
  
  # 5. 숫자형 범주 변수
  # numeric이지만 고유값 수가 2개 이상 10개 이하
  if (is.numeric(x) && n_unique >= 2 && n_unique <= 10) {
    return("숫자형 범주 변수")
  }
  
  # 6. 수치형 변수
  # numeric이고 고유값 수가 11개 이상
  if (is.numeric(x) && n_unique >= 11) {
    return("수치형 변수")
  }
  
  # 기준표에 명확히 들어가지 않는 변수
  return("기타 변수")
}


# ── 5. 전체 데이터프레임 변수 유형 자동 판정 ─────────────────────
detect_all_variable_types <- function(df) {
  
  result <- data.frame(
    variable = names(df),
    
    r_class = sapply(df, function(x) {
      paste(class(x), collapse = ", ")
    }),
    
    n_non_missing = sapply(df, function(x) {
      x <- normalize_missing(x)
      sum(!is.na(x))
    }),
    
    n_unique_non_missing = sapply(df, function(x) {
      x <- normalize_missing(x)
      length(unique(x[!is.na(x)]))
    }),
    
    unique_ratio = sapply(df, function(x) {
      x <- normalize_missing(x)
      n_nonmiss <- sum(!is.na(x))
      n_unique <- length(unique(x[!is.na(x)]))
      
      if (n_nonmiss == 0) {
        return(0)
      } else {
        return(round(n_unique / n_nonmiss, 3))
      }
    }),
    
    variable_type = sapply(df, detect_variable_type),
    
    is_time_candidate = sapply(df, is_date_convertible),
    
    row.names = NULL
  )
  
  return(result)
}