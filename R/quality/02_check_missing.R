# ============================================================================
# 02_check_missing.R
# 결측치 품질 검사
# ============================================================================
# [파일 역할]
# - 데이터 전체 결측률과 변수별 결측 집중 문제를 평가한다.
# - 최종 배점은 25점이다.
#
# [수정항목 반영 내역]
# - 수정항목 1) 품질 점수 계산 기준 보완
#   * 기존 기준은 전체 결측률과 결측률 40% 이상 변수 비율 중심이라,
#     전체 결측률이 낮으면 특정 변수에 결측이 몰려 있어도 점수가 과도하게 높게 나올 수 있었다.
#   * 이 파일에서는 다음 정보를 추가로 계산한다.
#       - 변수별 최대 결측률(max_missing_rate)
#       - 결측률 10% 이상 변수 수(missing_over_10_count)
#       - 결측률 20% 이상 변수 수(missing_over_20_count)
#       - 결측률 30% 이상 변수 수(missing_over_30_count)
#   * 예를 들어 airquality의 Ozone처럼 결측률이 20% 이상인 변수가 있으면
#     전체 결측률이 낮더라도 변수별 결측 패턴 점수에서 감점된다.
#
# - 수정항목 5) 예외 처리
#   * 행 수 0개, 열 수 0개인 데이터가 들어와도 계산 오류가 나지 않도록 처리한다.
# ============================================================================

check_missing_quality <- function(data) {
  n_row <- nrow(data)
  n_col <- ncol(data)
  total_cells <- n_row * n_col
  
  # 전체 결측률: 전체 셀 중 NA가 차지하는 비율
  total_missing_count <- if (total_cells == 0) 0 else sum(is.na(data))
  total_missing_rate <- quality_rate(total_missing_count, total_cells)
  total_missing_score <- score_missing_total(total_missing_rate)
  
  if (n_col == 0) {
    variable_missing <- data.frame(
      variable = character(0),
      missing_count = integer(0),
      missing_rate = numeric(0),
      missing_percent = numeric(0),
      missing_over_10 = logical(0),
      missing_over_20 = logical(0),
      missing_over_30 = logical(0),
      high_missing = logical(0),
      stringsAsFactors = FALSE
    )
  } else {
    missing_count <- vapply(seq_along(data), function(j) {
      sum(is.na(data[[j]]))
    }, integer(1))
    
    missing_rate <- if (n_row == 0) {
      rep(0, n_col)
    } else {
      missing_count / n_row
    }
    
    variable_missing <- data.frame(
      variable = names(data),
      missing_count = missing_count,
      missing_rate = round(missing_rate, 4),
      missing_percent = quality_percent(missing_rate),
      missing_over_10 = missing_rate >= 0.10,
      missing_over_20 = missing_rate >= 0.20,
      missing_over_30 = missing_rate >= 0.30,
      high_missing = missing_rate >= 0.40,
      stringsAsFactors = FALSE
    )
  }
  
  # 수정항목 1의 핵심 추가 지표
  max_missing_rate <- if (nrow(variable_missing) == 0) 0 else max(variable_missing$missing_rate, na.rm = TRUE)
  missing_over_10_count <- sum(variable_missing$missing_over_10)
  missing_over_20_count <- sum(variable_missing$missing_over_20)
  missing_over_30_count <- sum(variable_missing$missing_over_30)
  
  variable_pattern_score <- score_missing_variable_pattern(
    max_missing_rate = max_missing_rate,
    count_10 = missing_over_10_count,
    count_20 = missing_over_20_count,
    count_30 = missing_over_30_count
  )
  
  score <- total_missing_score + variable_pattern_score
  
  return(list(
    item = "결측치 품질",
    score = score,
    max_score = 25,
    total_missing_rate = total_missing_rate,
    max_missing_rate = max_missing_rate,
    details = list(
      total_missing_count = total_missing_count,
      total_cells = total_cells,
      total_missing_percent = quality_percent(total_missing_rate),
      max_missing_percent = quality_percent(max_missing_rate),
      missing_over_10_count = missing_over_10_count,
      missing_over_20_count = missing_over_20_count,
      missing_over_30_count = missing_over_30_count,
      variable_missing = variable_missing,
      sub_scores = data.frame(
        criterion = c("전체 결측률", "변수별 결측 집중 패턴"),
        score = c(total_missing_score, variable_pattern_score),
        max_score = c(15, 10),
        description = c(
          "전체 셀 중 결측값 비율을 기준으로 평가",
          "최대 변수 결측률과 결측률 10%/20%/30% 이상 변수 수를 함께 평가"
        ),
        stringsAsFactors = FALSE
      )
    )
  ))
}
