# ============================================================================
# 09_quality_visualization_helpers.R
# 품질 시각화 공통 helper 함수
# ============================================================================
# [파일 역할]
# - RDQAS는 상세 EDA 도구가 아니라, 분석 전에 데이터 품질과 분석 목적 적합성을
#   확인하는 서비스이다.
# - 따라서 시각화는 "전체 변수 탐색"이 아니라, 품질 문제가 두드러지는 변수만
#   요약해서 보여주는 방향으로 구성한다.
# - 이 파일은 다음 두 곳에서 공통으로 사용하는 helper 함수를 제공한다.
#     1) Shiny Step 3 품질 시각화 탭 (R/04_server.R)
#     2) HTML 보고서의 시각화 섹션 (R/02_report.R)
#   두 곳이 같은 helper를 사용하므로, 화면과 보고서의 시각화 기준이 항상 일치한다.
#
# [표시 기준]
# - 결측: 결측치가 있는 변수 중 결측률 상위 VIS_TOP_N개
# - 이상치: IQR 기준 이상치가 탐지된 수치형 변수 중 이상치 비율 상위 VIS_TOP_N개
#
# [의존성]
# - ggplot2, tidyr, dplyr (R/01_packages.R에서 이미 로드됨)
# - app.R는 R/quality 폴더의 *.R 파일을 파일명 순서로 source 하므로,
#   이 파일(09_)은 기존 품질 파일(01_~08_) 이후, suitability/report보다 먼저 로드된다.
# ============================================================================

# 시각화에 표시할 최대 변수 수
VIS_TOP_N <- 20

# ----------------------------------------------------------------------------
# 결측률 상위 변수 추출
# ----------------------------------------------------------------------------
# report$details$missing$details$variable_missing 에서
#   - missing_count > 0 인 변수만 남기고
#   - missing_percent 내림차순 정렬 후
#   - 상위 top_n 행을 data.frame으로 반환한다.
# 결측치가 있는 변수가 하나도 없으면 0행 data.frame을 반환한다.
get_top_missing_vars <- function(report, top_n = VIS_TOP_N) {
  empty_df <- data.frame(
    variable        = character(0),
    missing_count   = integer(0),
    missing_percent = numeric(0),
    stringsAsFactors = FALSE
  )

  miss_df <- tryCatch(
    report$details$missing$details$variable_missing,
    error = function(e) NULL
  )

  if (is.null(miss_df) || !is.data.frame(miss_df) || nrow(miss_df) == 0) {
    return(empty_df)
  }
  if (!all(c("variable", "missing_count", "missing_percent") %in% names(miss_df))) {
    return(empty_df)
  }

  # 결측치가 있는 변수만
  miss_df <- miss_df[!is.na(miss_df$missing_count) & miss_df$missing_count > 0, , drop = FALSE]
  if (nrow(miss_df) == 0) {
    return(empty_df)
  }

  # 결측률(missing_percent) 내림차순 정렬
  miss_df <- miss_df[order(-miss_df$missing_percent), , drop = FALSE]

  # 상위 top_n개
  if (nrow(miss_df) > top_n) {
    miss_df <- miss_df[seq_len(top_n), , drop = FALSE]
  }

  rownames(miss_df) <- NULL
  miss_df
}

# ----------------------------------------------------------------------------
# 이상치 비율 상위 변수명 추출
# ----------------------------------------------------------------------------
# report$details$outlier$details$outlier_table 에서
#   - outlier_count > 0 인 변수만 남기고
#   - outlier_percent가 있으면 그 값을 기준으로,
#     없으면 outlier_count / nrow(data) * 100 으로 비율을 계산해
#   - 이상치 비율 내림차순 상위 top_n개의 "변수명"을 반환한다.
# 실제 data에 존재하는 변수만 반환한다.
get_top_outlier_vars <- function(report, data, top_n = VIS_TOP_N) {
  out_tbl <- tryCatch(
    report$details$outlier$details$outlier_table,
    error = function(e) NULL
  )

  if (is.null(out_tbl) || !is.data.frame(out_tbl) || nrow(out_tbl) == 0) {
    return(character(0))
  }
  if (!all(c("variable", "outlier_count") %in% names(out_tbl))) {
    return(character(0))
  }

  # 이상치가 탐지된 변수만
  out_tbl <- out_tbl[!is.na(out_tbl$outlier_count) & out_tbl$outlier_count > 0, , drop = FALSE]
  if (nrow(out_tbl) == 0) {
    return(character(0))
  }

  # 정렬 기준이 되는 비율 계산
  if ("outlier_percent" %in% names(out_tbl) && any(!is.na(out_tbl$outlier_percent))) {
    sort_pct <- out_tbl$outlier_percent
  } else {
    n_row <- if (is.null(data)) NA_integer_ else nrow(data)
    sort_pct <- if (is.na(n_row) || n_row == 0) {
      rep(0, nrow(out_tbl))
    } else {
      out_tbl$outlier_count / n_row * 100
    }
  }
  sort_pct[is.na(sort_pct)] <- 0

  # 이상치 비율 내림차순 정렬
  ord <- order(-sort_pct)
  out_tbl <- out_tbl[ord, , drop = FALSE]

  vars <- as.character(out_tbl$variable)

  # 실제 data에 존재하는 변수만
  if (!is.null(data)) {
    vars <- vars[vars %in% names(data)]
  }
  vars <- vars[!is.na(vars) & nchar(vars) > 0]

  # 상위 top_n개
  if (length(vars) > top_n) {
    vars <- vars[seq_len(top_n)]
  }

  vars
}

# ----------------------------------------------------------------------------
# 결측률 막대그래프
# ----------------------------------------------------------------------------
# miss_df: get_top_missing_vars()의 반환값
#          (variable, missing_count, missing_percent 컬럼 포함)
# - missing_percent 기준으로 정렬된 막대그래프
# - coord_flip()으로 가로 막대 형태
make_missing_plot <- function(miss_df) {
  miss_df <- as.data.frame(miss_df, stringsAsFactors = FALSE)

  # missing_percent 기준 정렬 (낮은 값이 아래, 높은 값이 위로 오도록 reorder)
  ggplot2::ggplot(
    miss_df,
    ggplot2::aes(
      x    = stats::reorder(variable, missing_percent),
      y    = missing_percent,
      fill = ifelse(missing_percent >= 10, "#E24B4A", "#f5821d")
    )
  ) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(missing_percent, "%")),
      hjust = -0.1, size = 3.5
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      limits = c(0, max(miss_df$missing_percent, na.rm = TRUE) * 1.3)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "결측률 상위 변수",
      x = NULL, y = "결측률 (%)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.margin        = ggplot2::margin(8, 32, 8, 8)
    )
}

# ----------------------------------------------------------------------------
# 이상치 박스플롯 (facet_wrap, 여러 줄 배치)
# ----------------------------------------------------------------------------
# data:         원본 데이터프레임
# outlier_vars: get_top_outlier_vars()의 반환값(변수명 벡터)
# ncol:         facet 열 수 (기본 4)
# - 변수별 단위가 다르므로 scales = "free_y" 사용
# - 한 줄에 나열하지 않고 facet_wrap으로 여러 줄 배치
make_outlier_boxplot <- function(data, outlier_vars, ncol = 4) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)

  # data에 실제 존재하는 변수만 사용
  outlier_vars <- outlier_vars[outlier_vars %in% names(data)]

  plot_df <- data[, outlier_vars, drop = FALSE]

  # 박스플롯 대상은 수치형이어야 하므로 수치형으로 변환
  plot_df[] <- lapply(plot_df, function(x) suppressWarnings(as.numeric(x)))

  # long format 변환
  plot_long <- tidyr::pivot_longer(
    plot_df,
    cols      = tidyr::everything(),
    names_to  = "variable",
    values_to = "value"
  )

  # NA / 비유한값 제거
  plot_long <- plot_long[!is.na(plot_long$value) & is.finite(plot_long$value), , drop = FALSE]

  # facet 순서를 outlier_vars(이상치 비율 상위 순) 순서로 고정
  plot_long$variable <- factor(plot_long$variable, levels = outlier_vars)

  ggplot2::ggplot(plot_long, ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot(
      fill          = "#B5D4F4",
      color         = "#185FA5",
      outlier.color = "#E24B4A",
      outlier.alpha = 0.6,
      width         = 0.5
    ) +
    ggplot2::facet_wrap(~ variable, ncol = ncol, scales = "free_y") +
    ggplot2::labs(
      title = "이상치 비율 상위 수치형 변수 (IQR 기준)",
      x = NULL, y = "값"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_blank(),
      axis.ticks.x       = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(face = "bold")
    )
}
