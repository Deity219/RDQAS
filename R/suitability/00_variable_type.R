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


# ── 4. ID성 변수로 의심되는 변수명인지 판정 ─────────────────────
# 변수명에 id, no, code 등의 토큰이 포함된 경우에만 ID성 변수로 본다.
# (CRIM, KS11.Open 같은 측정 변수가 고유값 비율만으로 ID성으로
#  오판되는 문제를 막기 위해, ID성 판정은 변수명 기반으로 제한한다.)
is_id_like_name <- function(var_name) {
  if (is.null(var_name) || length(var_name) == 0) {
    return(FALSE)
  }
  
  var_name <- as.character(var_name)[1]
  
  if (is.na(var_name) || !nzchar(trimws(var_name))) {
    return(FALSE)
  }
  
  nm <- tolower(trimws(var_name))
  
  # id / no / code / key / seq 를 토큰 경계 기준으로 매칭
  grepl("(^|[^a-z])(id|no|code|key|seq)([^a-z]|$)", nm)
}


# ── 5. 단일 변수 유형 자동 판정 ─────────────────────────────────
# var_name: 변수명(있으면 ID성 변수 판정에 사용)
detect_variable_type <- function(x, var_name = NULL) {
  
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
  # 날짜형은 수치형/ID성 변수보다 먼저 판정해야 함
  if (is_date_convertible(x)) {
    return("날짜형 변수")
  }
  
  # 3. 수치형 변수 판정 (ID성 변수 판정보다 먼저)
  # numeric 변수는 고유값 비율만으로 ID성으로 오판되지 않도록 먼저 처리한다.
  if (is.numeric(x)) {
    
    # 3-1. 변수명이 명확히 ID성(id/no/code 등)이고 거의 모든 값이 고유하면
    #      숫자형 ID 컬럼(예: 일련번호)으로 보아 ID성 변수로 판정
    if (is_id_like_name(var_name) && n_nonmiss >= 30 && unique_ratio >= 0.9) {
      return("ID성 변수")
    }
    
    # 3-2. 고유값 수가 2개 이상 10개 이하 → 숫자형 범주 변수
    if (n_unique >= 2 && n_unique <= 10) {
      return("숫자형 범주 변수")
    }
    
    # 3-3. 고유값 수가 11개 이상 → 수치형 변수
    if (n_unique >= 11) {
      return("수치형 변수")
    }
    
    return("기타 변수")
  }
  
  # 4. ID성 변수 (numeric이 아닌 변수)
  # 변수명이 id/no/code 등을 포함하고,
  # 결측 제외 관측치 수가 30개 이상이며 고유값 비율이 0.9 이상인 경우에만 판정
  if (is_id_like_name(var_name) && n_nonmiss >= 30 && unique_ratio >= 0.9) {
    return("ID성 변수")
  }
  
  # 5. 범주형 변수
  # 문자형, factor형, logical형 변수 중 고유값 수가 2개 이상 10개 이하
  if ((is.character(x) || is.factor(x) || is.logical(x)) &&
      n_unique >= 2 && n_unique <= 10) {
    return("범주형 변수")
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
    
    # 변수명을 함께 전달하여 ID성 변수 판정에 활용
    variable_type = mapply(
      function(col, nm) detect_variable_type(col, var_name = nm),
      df, names(df),
      SIMPLIFY = TRUE, USE.NAMES = FALSE
    ),
    
    is_time_candidate = sapply(df, is_date_convertible),
    
    row.names = NULL
  )
  
  return(result)
}