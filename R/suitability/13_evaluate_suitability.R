# ============================================================================
# TODO: 적합성 평가 함수들 추가 필요
# 각 분석 목적별로 커스터마이즈된 평가 기준 정의
# - basic_stats: 기초 통계 분석 적합성
# - visualization: 시각화 적합성
# - advanced_ml: 가설 검정 적합성
# - correlation: 상관관계 분석 적합성
# - regression: 회귀분석 적합성
# - classification: 분류분석 적합성
# - cluster: 군집분석 적합성
# - time_series: 시계열 분석 적합성
# - dimension: 차원 축소 적합성
# - survival: 생존 분석 적합성
# ============================================================================

evaluate_suitability <- function(quality_report, purpose) {
  score <- quality_report$score
  
  suitability <- list()
  
  # 기본 설정 (모든 분석에 공통으로 적용)
  if (is.null(purpose) || purpose == "") {
    return(list(
      status = "미선택",
      color = "secondary",
      message = "분석 목적을 선택해주세요.",
      recommendations = c()
    ))
  }
  
  # TODO: 각 분석별 적합성 평가 함수 구현
  # 현재는 기본 평가만 제공
  if (purpose == "basic_stats") {
    if (score >= 80) {
      suitability$status <- "적합"
      suitability$color <- "success"
      suitability$message <- "기초 통계 분석에 충분히 적합합니다."
      suitability$recommendations <- c(
        "데이터를 그대로 사용해도 무방합니다.",
        "결측치가 있는 변수는 제거 또는 대체 고려"
      )
    } else if (score >= 60) {
      suitability$status <- "부분적 적합"
      suitability$color <- "warning"
      suitability$message <- "기초 통계 분석에 사용 가능하지만 주의가 필요합니다."
      suitability$recommendations <- c(
        "결측치 처리 필요",
        "이상치 검토 및 처리 권장",
        "데이터 정제 후 사용 권장"
      )
    } else {
      suitability$status <- "부적합"
      suitability$color <- "danger"
      suitability$message <- "현 상태로는 분석에 사용하기 어렵습니다."
      suitability$recommendations <- c(
        "결측치 대체 또는 제거",
        "이상치 처리",
        "데이터 표준화",
        "형식 통일"
      )
    }
  } else {
    # TODO: 다른 분석 목적별 적합성 평가 추가
    suitability$status <- "평가 예정"
    suitability$color <- "info"
    suitability$message <- "해당 분석의 적합성 평가는 준비 중입니다."
    suitability$recommendations <- c(
      "분석을 진행하기 전에 데이터를 충분히 검토하세요.",
      "결측치와 이상치를 확인하세요."
    )
  }
  
  return(suitability)
}