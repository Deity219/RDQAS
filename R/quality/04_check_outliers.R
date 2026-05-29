# ============================================================================
# 04_check_outliers.R
# 이상치 품질 검사
# ============================================================================
# [파일 역할]
# - 수치형 변수의 IQR 기준 이상치 비율을 평가한다.
# - 최종 배점은 15점이다.
#
# [수정항목 반영 내역]
# - 수정항목 5) 예외 처리
#   * 수치형 변수가 없는 경우 앱이 멈추지 않고 15점으로 처리한다.
#   * 모든 값이 결측인 변수, Inf/-Inf가 포함된 변수도 IQR 계산에서 오류가 나지 않도록
#     유한한 값만 사용한다.
#
# [기존 기준 유지]
# - numeric이고 고유값 수가 11개 이상인 "수치형 변수"만 검사한다.
# - numeric이지만 고유값 수가 2~10개인 수치형 범주 변수는 이상치 검사 대상에서 제외한다.
# - NaN/Inf/-Inf는 06_check_format_consistency.R에서 별도로 점검한다.
# ============================================================================

check_outliers_quality <- function(data, variable_types) {
  numeric_idx <- which(variable_types$variable_type == "수치형 변수")
  
  if (length(numeric_idx) == 0) {
    return(list(
      item = "이상치 품질",
      score = 15,
      max_score = 15,
      mean_outlier_rate = 0,
      skipped = TRUE,
      warning = "수치형 변수가 없어 이상치 검사를 수행하지 않았습니다.",
      details = list(
        outlier_table = data.frame(
          variable = character(0),
          outlier_count = integer(0),
          non_missing_count = integer(0),
          outlier_rate = numeric(0),
          outlier_percent = numeric(0),
          lower_bound = numeric(0),
          upper_bound = numeric(0),
          stringsAsFactors = FALSE
        )
      )
    ))
  }
  
  outlier_list <- lapply(numeric_idx, function(j) {
    x <- data[[j]]
    
    if (!is.numeric(x) && !is.integer(x)) {
      return(data.frame(
        variable = variable_types$variable[j],
        outlier_count = 0,
        non_missing_count = 0,
        outlier_rate = 0,
        outlier_percent = 0,
        lower_bound = NA_real_,
        upper_bound = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    
    # 수정항목 5 반영:
    # NA/NaN/Inf/-Inf는 IQR 계산에서 제외한다.
    # 특수값 자체는 06_check_format_consistency.R에서 따로 점검한다.
    x_valid <- x[!is.na(x) & is.finite(x)]
    n_valid <- length(x_valid)
    
    if (n_valid == 0) {
      outlier_count <- 0
      outlier_rate <- 0
      lower_bound <- NA_real_
      upper_bound <- NA_real_
    } else {
      q1 <- as.numeric(quantile(x_valid, probs = 0.25, na.rm = TRUE, names = FALSE))
      q3 <- as.numeric(quantile(x_valid, probs = 0.75, na.rm = TRUE, names = FALSE))
      iqr_value <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr_value
      upper_bound <- q3 + 1.5 * iqr_value
      
      if (is.na(iqr_value) || iqr_value == 0) {
        outlier_count <- 0
      } else {
        outlier_count <- sum(x_valid < lower_bound | x_valid > upper_bound)
      }
      
      outlier_rate <- quality_rate(outlier_count, n_valid)
    }
    
    data.frame(
      variable = variable_types$variable[j],
      outlier_count = outlier_count,
      non_missing_count = n_valid,
      outlier_rate = round(outlier_rate, 4),
      outlier_percent = quality_percent(outlier_rate),
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      stringsAsFactors = FALSE
    )
  })
  
  outlier_table <- do.call(rbind, outlier_list)
  mean_outlier_rate <- mean(outlier_table$outlier_rate, na.rm = TRUE)
  score <- score_outlier(mean_outlier_rate)
  
  return(list(
    item = "이상치 품질",
    score = score,
    max_score = 15,
    mean_outlier_rate = mean_outlier_rate,
    skipped = FALSE,
    warning = NULL,
    details = list(
      mean_outlier_percent = quality_percent(mean_outlier_rate),
      outlier_table = outlier_table
    )
  ))
}
