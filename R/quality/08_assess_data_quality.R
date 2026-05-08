# ============================================================================
# 데이터 품질 진단 함수들
# ============================================================================

assess_data_quality <- function(data) {
  quality_report <- list()
  
  # 1. 결측치 분석
  missing_analysis <- data.frame(
    variable = names(data),
    missing_count = colSums(is.na(data)),
    missing_percent = round(colMeans(is.na(data)) * 100, 2)
  ) %>%
    filter(missing_count > 0) %>%
    arrange(desc(missing_percent))
  
  # 2. 이상치 분석 (수치형 변수)
  outlier_analysis <- data.frame()
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR
      outlier_count <- sum(data[[col]] < lower_bound | data[[col]] > upper_bound, na.rm = TRUE)
      
      if (outlier_count > 0) {
        outlier_analysis <- rbind(outlier_analysis, data.frame(
          variable = col,
          outlier_count = outlier_count,
          outlier_percent = round(outlier_count / nrow(data) * 100, 2)
        ))
      }
    }
  }
  
  # 3. 값 형식 일관성 분석 (문자형 변수)
  format_analysis <- data.frame()
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      unique_vals <- length(unique(na.omit(data[[col]])))
      total_vals <- sum(!is.na(data[[col]]))
      
      # 대소문자 혼재 확인
      vals <- na.omit(as.character(data[[col]]))
      vals <- trimws(vals)
      
      if (length(vals) == 0) {
        has_case_mix <- FALSE
      } else {
        lower_vals <- tolower(vals)
        case_check <- tapply(vals, lower_vals, function(x) length(unique(x)) > 1)
        has_case_mix <- any(case_check)
      }
      
      if (has_case_mix | unique_vals > total_vals * 0.5) {
        format_analysis <- rbind(format_analysis, data.frame(
          variable = col,
          unique_values = unique_vals,
          issue = if(has_case_mix) "대소문자 혼재" else "높은 고유값"
        ))
      }
    }
  }
  
  # 품질 점수 계산 (0-100)
  quality_score <- 100
  
  # 결측치에 따른 감점 (최대 30점)
  if (nrow(missing_analysis) > 0) {
    max_missing <- max(missing_analysis$missing_percent)
    quality_score <- quality_score - min(30, max_missing / 2)
  }
  
  # 이상치에 따른 감점 (최대 20점)
  if (nrow(outlier_analysis) > 0) {
    max_outlier <- max(outlier_analysis$outlier_percent)
    quality_score <- quality_score - min(20, max_outlier / 2)
  }
  
  # 형식 일관성에 따른 감점 (최대 15점)
  if (nrow(format_analysis) > 0) {
    quality_score <- quality_score - min(15, nrow(format_analysis) * 5)
  }
  
  quality_score <- round(max(0, quality_score), 0)
  
  quality_report$score <- quality_score
  quality_report$missing <- missing_analysis
  quality_report$outliers <- outlier_analysis
  quality_report$format <- format_analysis
  
  return(quality_report)
}