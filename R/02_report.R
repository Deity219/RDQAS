# ============================================================================
# HTML 보고서 생성 함수
# ============================================================================
# 입력값:
#   file        : 저장 경로 (downloadHandler의 file 인자)
#   data        : 원본 데이터프레임
#   report      : assess_data_quality() 반환값
#                 - $total_score   : 품질 점수 (0~100 정수)
#                 - $final_status  : 최종 판정 문자열
#                 - $major_issues  : data.frame(문제항목, 상태, 설명)
#                 - $warnings      : character vector
#                 - $details$missing$details$variable_missing
#                     : data.frame(variable, missing_count, missing_rate, missing_percent, high_missing)
#                 - $details$outlier$details$outlier_table
#                     : data.frame(variable, outlier_count, outlier_rate, outlier_percent, ...)
#                 - $details$type$details$type_issue_table
#                     : data.frame(variable, issue, ...)
#                 - $details$duplicate$details$duplicate_count
#   suitability : evaluate_suitability() 반환값
#                 - $status        : 상태 문자열
#                 - $color         : "success" | "warning" | "danger" | "info" | "secondary"
#                 - $message       : 평가 메시지
#                 - $recommendations : 권장사항 character vector
#   purpose     : 분석 목적 선택값 (input$analysis_purpose)
#   filename    : 업로드한 파일명 (표시용)
# ============================================================================

generate_html_report <- function(file, data, report, suitability,
                                 purpose = "", filename = "데이터") {
  
  # --------------------------------------------------------------------------
  # 내부 헬퍼 함수
  # --------------------------------------------------------------------------
  
  # HTML 특수문자 이스케이프
  esc <- function(x) {
    x <- as.character(x)
    x <- gsub("&",  "&amp;",  x, fixed = TRUE)
    x <- gsub("<",  "&lt;",   x, fixed = TRUE)
    x <- gsub(">",  "&gt;",   x, fixed = TRUE)
    x <- gsub('"',  "&quot;", x, fixed = TRUE)
    x
  }
  
  # 점수에 따른 색상 반환
  score_color <- function(s) {
    if (is.na(s) || !is.numeric(s)) return("#aaaaaa")
    if (s >= 90) return("#1D9E75")
    if (s >= 70) return("#2196F3")
    if (s >= 50) return("#f5821d")
    return("#E24B4A")
  }
  
  # 점수에 따른 등급 문자 반환
  score_grade <- function(s) {
    if (is.na(s) || !is.numeric(s)) return("알 수 없음")
    if (s >= 90) return("우수")
    if (s >= 70) return("양호")
    if (s >= 50) return("주의")
    return("부적합합")
  }
  
  # SVG 도넛 차트 생성 (외부 라이브러리 없이 순수 SVG)
  make_donut_svg <- function(s, color) {
    radius    <- 70
    cx        <- 100
    cy        <- 100
    stroke_w  <- 22
    full_circ <- 2 * pi * radius
    filled    <- full_circ * (s / 100)
    gap       <- full_circ - filled
    
    track <- paste0(
      "<circle cx='", cx, "' cy='", cy, "' r='", radius, "' ",
      "fill='none' stroke='#e9ecef' stroke-width='", stroke_w, "'/>"
    )
    arc <- paste0(
      "<circle cx='", cx, "' cy='", cy, "' r='", radius, "' ",
      "fill='none' stroke='", color, "' stroke-width='", stroke_w, "' ",
      "stroke-dasharray='", round(filled, 2), " ", round(gap, 2), "' ",
      "stroke-dashoffset='", round(full_circ * 0.25, 2), "' ",
      "stroke-linecap='round' ",
      "transform='rotate(-90 ", cx, " ", cy, ")'/>"
    )
    score_text <- paste0(
      "<text x='", cx, "' y='", cy - 8, "' ",
      "text-anchor='middle' dominant-baseline='middle' ",
      "font-size='32' font-weight='bold' fill='", color, "' ",
      "font-family='Apple SD Gothic Neo, Malgun Gothic, Arial, sans-serif'>",
      s, "</text>"
    )
    sub_text <- paste0(
      "<text x='", cx, "' y='", cy + 22, "' ",
      "text-anchor='middle' dominant-baseline='middle' ",
      "font-size='13' fill='#999' ",
      "font-family='Apple SD Gothic Neo, Malgun Gothic, Arial, sans-serif'>",
      "/ 100</text>"
    )
    paste0(
      "<svg viewBox='0 0 200 200' width='200' height='200' ",
      "xmlns='http://www.w3.org/2000/svg'>",
      track, arc, score_text, sub_text, "</svg>"
    )
  }
  
  # suitability가 NULL이거나 구조가 잘못된 경우 기본값으로 대체
  if (is.null(suitability) || is.null(suitability$status)) {
    suitability <- list(
      status          = "평가 미완료",
      color           = "secondary",
      message         = "적합성 평가 결과가 없습니다.",
      recommendations = character()
    )
  }
  
  # suitability$status → 점수 변환 (도넛 차트용)
  suit_score <- function(status) {
    switch(as.character(status),
           "적합"      = 100,
           "부분 적합" = 60,
           "부적합"    = 20,
           "미선택"    = NA,
           "평가 미완료" = NA,
           NA
    )
  }
  
  # suitability$color → 도넛 색상 매핑
  suit_donut_color <- function(color) {
    switch(color,
           success   = "#1D9E75",
           warning   = "#f5821d",
           danger    = "#E24B4A",
           info      = "#2196F3",
           "#aaaaaa"
    )
  }
  
  # suitability$color → CSS 클래스 색상 매핑
  suit_bg_color <- function(color) {
    switch(color,
           success   = "background:#d4edda; border-left:5px solid #28a745; color:#155724;",
           warning   = "background:#fff3cd; border-left:5px solid #ffc107; color:#856404;",
           danger    = "background:#f8d7da; border-left:5px solid #dc3545; color:#721c24;",
           info      = "background:#d1ecf1; border-left:5px solid #17a2b8; color:#0c5460;",
           secondary = "background:#e2e3e5; border-left:5px solid #6c757d; color:#383d41;",
           "background:#e2e3e5; border-left:5px solid #6c757d; color:#383d41;"
    )
  }
  
  # 분석 목적 한글 레이블
  purpose_label <- function(p) {
    labels <- c(
      basic_stats   = "기초 통계 분석",
      visualization = "범주형 변수 빈도 분석",
      advanced_ml   = "통계적 검정 / 가설 검정",
      correlation   = "상관관계 분석",
      regression    = "회귀분석",
      classification = "분류분석",
      cluster       = "군집분석",
      time_series   = "시계열 분석",
      dimension     = "차원 축소 / 변수 구조 탐색",
      survival      = "생존 분석"
    )
    if (!is.null(labels[p]) && p %in% names(labels)) labels[[p]] else "미선택"
  }
  
  # base64로 ggplot 이미지 인코딩 (외부 파일 없이 HTML에 삽입)
  plot_to_base64 <- function(plot_obj, width = 7, height = 3.5) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp))
    ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height,
                    dpi = 120, bg = "white")
    base64enc::base64encode(tmp)
  }
  
  # --------------------------------------------------------------------------
  # assess_data_quality() 반환 구조에서 필요한 데이터 추출
  # --------------------------------------------------------------------------
  
  # 점수
  score <- report$total_score
  
  # 결측치 테이블: variable, missing_count, missing_percent
  missing_df <- report$details$missing$details$variable_missing
  missing_df <- missing_df[missing_df$missing_count > 0, ]
  
  # 이상치 테이블: variable, outlier_count, outlier_percent
  outlier_df <- report$details$outlier$details$outlier_table
  outlier_df <- outlier_df[outlier_df$outlier_count > 0, ]
  
  # 자료형 오류 테이블: variable, issue
  type_df <- report$details$type$details$type_issue_table
  
  # 중복 행 수
  n_dup_rows <- report$details$duplicate$details$duplicate_count
  
  # 주요 이슈 (위험/주의 항목)
  major_issues <- report$major_issues
  
  # 기본 정보
  n_rows          <- nrow(data)
  n_cols          <- ncol(data)
  n_missing_cells <- sum(is.na(data))
  
  grade   <- score_grade(score)
  s_color <- score_color(score)
  p_label <- purpose_label(purpose)
  
  # --------------------------------------------------------------------------
  # 시각화 생성
  # --------------------------------------------------------------------------
  
  # 결측치 막대그래프 (결측치 있을 때만)
  missing_plot_tag <- ""
  if (!is.null(missing_df) && nrow(missing_df) > 0) {
    miss_df <- missing_df
    p_miss <- ggplot2::ggplot(
      miss_df,
      ggplot2::aes(
        x    = reorder(variable, missing_percent),
        y    = missing_percent,
        fill = ifelse(missing_percent >= 10, "#E24B4A", "#f5821d")
      )
    ) +
      ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(missing_percent, "%")),
        hjust = -0.1, size = 3.5
      ) +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_y_continuous(
        limits = c(0, max(miss_df$missing_percent, na.rm = TRUE) * 1.3)
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = "결측률 (%)") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        plot.margin        = ggplot2::margin(8, 32, 8, 8)
      )
    
    tryCatch({
      img_b64 <- plot_to_base64(p_miss)
      missing_plot_tag <- paste0(
        "<img src='data:image/png;base64,", img_b64,
        "' style='max-width:100%; height:auto;' alt='결측치 분포'>"
      )
    }, error = function(e) {
      missing_plot_tag <<- "<p style='color:#999;'>시각화를 생성할 수 없습니다.</p>"
    })
  }
  
  # 수치형 변수 박스플롯 (수치형 변수 있을 때만)
  boxplot_tag <- ""
  num_cols <- if (!is.null(report$variable_types) &&
                  "variable_type" %in% names(report$variable_types)) {
    report$variable_types$variable[
      report$variable_types$variable_type == "수치형 변수"
    ]
  } else {
    names(data)[sapply(data, is.numeric)]
  }
  if (length(num_cols) > 0) {
    plot_df <- data[, num_cols, drop = FALSE]
    plot_long <- tidyr::pivot_longer(
      plot_df,
      cols      = dplyr::everything(),
      names_to  = "variable",
      values_to = "value"
    )
    plot_long <- plot_long[!is.na(plot_long$value), ]
    
    p_box <- ggplot2::ggplot(plot_long, ggplot2::aes(x = variable, y = value)) +
      ggplot2::geom_boxplot(
        fill            = "#B5D4F4",
        color           = "#185FA5",
        outlier.color   = "#E24B4A",
        outlier.alpha   = 0.6,
        width           = 0.5
      ) +
      ggplot2::labs(x = NULL, y = "값") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_text(angle = 30, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      )
    
    tryCatch({
      box_height <- max(3.5, length(num_cols) * 0.5)
      img_b64    <- plot_to_base64(p_box, height = box_height)
      boxplot_tag <- paste0(
        "<img src='data:image/png;base64,", img_b64,
        "' style='max-width:100%; height:auto;' alt='수치형 변수 박스플롯'>"
      )
    }, error = function(e) {
      boxplot_tag <<- "<p style='color:#999;'>시각화를 생성할 수 없습니다.</p>"
    })
  }
  
  # --------------------------------------------------------------------------
  col_rows_html <- ""
  for (col in names(data)) {
    col_data    <- data[[col]]
    type_label  <- if (is.numeric(col_data)) "수치형"
    else if (inherits(col_data, "Date")) "날짜형"
    else "문자형"
    
    miss_pct <- round(mean(is.na(col_data)) * 100, 1)
    dup_pct  <- round(mean(duplicated(col_data[!is.na(col_data)])) * 100, 1)
    
    # 이상치 (수치형만)
    outlier_str <- "—"
    if (is.numeric(col_data)) {
      Q1  <- quantile(col_data, 0.25, na.rm = TRUE)
      Q3  <- quantile(col_data, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      n_out <- sum(col_data < Q1 - 1.5 * IQR | col_data > Q3 + 1.5 * IQR, na.rm = TRUE)
      if (n_out > 0) outlier_str <- as.character(n_out)
    }
    
    # 행 상태 색상
    row_style <- if (miss_pct >= 10) "background:#FCEBEB; color:#A32D2D;"
    else if (miss_pct > 0 || outlier_str != "—") "background:#FAEEDA; color:#854F0B;"
    else ""
    
    col_rows_html <- paste0(col_rows_html, "
      <tr style='", row_style, "'>
        <td>", esc(col),        "</td>
        <td>", type_label,      "</td>
        <td>", miss_pct, "%",   "</td>
        <td>", dup_pct,  "%",   "</td>
        <td>", outlier_str,     "</td>
      </tr>")
  }
  
  # --------------------------------------------------------------------------
  # 주요 발견사항 생성 (major_issues + 중복 행 기반)
  # --------------------------------------------------------------------------
  findings_html <- ""
  
  # major_issues 테이블에서 위험/주의 항목 출력
  if (!is.null(major_issues) && nrow(major_issues) > 0 &&
      !(nrow(major_issues) == 1 && major_issues$문제항목[1] == "없음")) {
    
    for (i in seq_len(nrow(major_issues))) {
      r <- major_issues[i, ]
      box_style <- if (r$상태 == "위험") {
        "background:#f8d7da; border-left:4px solid #dc3545; padding:12px 16px; margin:10px 0; border-radius:4px; color:#721c24;"
      } else {
        "background:#fff3cd; border-left:4px solid #ffc107; padding:12px 16px; margin:10px 0; border-radius:4px; color:#856404;"
      }
      icon <- if (r$상태 == "위험") "⚠️" else "🔔"
      findings_html <- paste0(findings_html,
                              "<div style='", box_style, "'>",
                              icon, " <strong>[", esc(r$상태), "] ", esc(r$문제항목), "</strong><br>",
                              esc(r$설명),
                              "</div>"
      )
    }
  }
  
  # 중복 행
  if (!is.null(n_dup_rows) && n_dup_rows > 0) {
    findings_html <- paste0(findings_html,
                            "<div style='background:#fff3cd; border-left:4px solid #ffc107;
                   padding:12px 16px; margin:10px 0; border-radius:4px; color:#856404;'>
        🔔 <strong>중복 행 ", n_dup_rows, "건:</strong>
        분석 목적에 따라 중복 제거 여부를 결정하세요.
      </div>"
    )
  }
  
  # 경고 문구 (warnings)
  if (!is.null(report$warnings) && length(report$warnings) > 0) {
    for (w in report$warnings) {
      findings_html <- paste0(findings_html,
                              "<div style='background:#e2e3e5; border-left:4px solid #6c757d;
                     padding:12px 16px; margin:10px 0; border-radius:4px; color:#383d41;'>
          ℹ️ ", esc(w), "
        </div>"
      )
    }
  }
  
  # 아무 문제 없을 때
  if (findings_html == "") {
    findings_html <- "
      <div style='background:#d4edda; border-left:4px solid #28a745;
                  padding:12px 16px; margin:10px 0; border-radius:4px; color:#155724;'>
        ✅ 주요 품질 문제가 발견되지 않았습니다.
      </div>"
  }
  
  # --------------------------------------------------------------------------
  # 권장사항 목록
  # --------------------------------------------------------------------------
  recs_html <- ""
  if (!is.null(suitability$recommendations) && length(suitability$recommendations) > 0) {
    items <- paste0(
      "<li style='margin:6px 0;'>", esc(suitability$recommendations), "</li>",
      collapse = "\n"
    )
    recs_html <- paste0("<ul style='padding-left:20px; margin:10px 0;'>", items, "</ul>")
  }
  
  # --------------------------------------------------------------------------
  # HTML 조립
  # --------------------------------------------------------------------------
  html <- paste0('
<!DOCTYPE html>
<html lang="ko">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>RDQAS 데이터 품질 진단 보고서</title>
  <style>
    * { box-sizing: border-box; }
    body {
      font-family: "Apple SD Gothic Neo", "Malgun Gothic", Arial, sans-serif;
      line-height: 1.6;
      color: #333;
      margin: 0;
      padding: 20px;
      background: #f0f2f5;
    }
    .container {
      max-width: 960px;
      margin: 0 auto;
      background: #fff;
      border-radius: 10px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.1);
      overflow: hidden;
    }
    /* 헤더 */
    .report-header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 36px 40px 28px;
    }
    .report-header h1 { margin: 0 0 8px; font-size: 26px; }
    .report-meta {
      font-size: 13px;
      opacity: 0.85;
      display: flex;
      gap: 16px;
      flex-wrap: wrap;
      margin-top: 6px;
    }
    /* 본문 */
    .body-content { padding: 36px 40px; }
    /* 섹션 */
    .section { margin-bottom: 40px; }
    .section-title {
      font-size: 17px;
      font-weight: bold;
      color: #333;
      border-bottom: 3px solid #667eea;
      padding-bottom: 8px;
      margin-bottom: 20px;
    }
    /* 요약 카드 */
    .card-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
      gap: 16px;
      margin-bottom: 24px;
    }
    .metric-card {
      background: #f8f9fa;
      border-radius: 8px;
      padding: 18px 16px;
      text-align: center;
      border: 1px solid #e9ecef;
    }
    .metric-label { font-size: 12px; color: #888; margin-bottom: 6px; }
    .metric-value { font-size: 26px; font-weight: bold; color: #333; }
    .metric-value.warn { color: #E24B4A; }
    /* 도넛 두 개 나란히 */
    .donut-pair {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 16px;
      margin-bottom: 16px;
    }
    /* 도넛 차트 영역 */
    .score-donut-wrap {
      display: flex;
      flex-direction: column;
      align-items: center;
      gap: 12px;
      padding: 20px 16px;
      background: #f8f9fa;
      border-radius: 8px;
      border-top: 5px solid ', s_color, ';
      margin-bottom: 16px;
      text-align: center;
    }
    .donut-chart { flex-shrink: 0; }
    .donut-info  { width: 100%; }
    .donut-grade {
      font-size: 16px;
      font-weight: bold;
      word-break: keep-all;
    }
    .donut-legend { font-size: 12px; color: #555; line-height: 2; word-break: keep-all; }
    .legend-dot {
      display: inline-block;
      width: 10px; height: 10px;
      border-radius: 50%;
      margin-right: 4px;
      vertical-align: middle;
    }
    /* PDF 인쇄 버튼 */
    .print-btn-wrap {
      text-align: right;
      margin-bottom: 12px;
    }
    .print-btn {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      padding: 8px 18px;
      background: #667eea;
      color: white;
      border: none;
      border-radius: 6px;
      font-size: 14px;
      font-family: inherit;
      cursor: pointer;
    }
    .print-btn:hover { background: #5568d3; }
    @media print {
      .print-btn-wrap { display: none; }
      body { background: white; padding: 0; }
      .container { box-shadow: none; border-radius: 0; }
      .report-header {
        background: #5a67d8 !important;
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
        color: white !important;
      }
      .report-header h1 { color: white !important; }
      .report-meta { color: white !important; opacity: 1 !important; }
      .report-meta span { color: white !important; }
      /* 도넛 두 개 나란히 고정 */
      .donut-pair {
        display: grid !important;
        grid-template-columns: 1fr 1fr !important;
        gap: 16px !important;
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
      }
      .score-donut-wrap {
        display: flex !important;
        flex-direction: column !important;
        align-items: center !important;
        text-align: center !important;
        page-break-inside: avoid !important;
        -webkit-print-color-adjust: exact !important;
        print-color-adjust: exact !important;
      }
      .donut-chart {
        flex-shrink: 0 !important;
      }
      .donut-grade {
        font-size: 14px !important;
        word-break: keep-all !important;
        white-space: normal !important;
      }
      .donut-legend {
        font-size: 11px !important;
        line-height: 1.8 !important;
        word-break: keep-all !important;
      }
    }
    /* 경고 박스 */
    .warn-box {
      background: #fff3cd;
      border-left: 4px solid #ffc107;
      padding: 14px 18px;
      margin: 12px 0;
      border-radius: 4px;
      color: #856404;
    }
    .warn-box-title { font-weight: bold; margin-bottom: 6px; }
    .warn-item { margin: 4px 0; }
    .ok-box {
      background: #d4edda;
      border-left: 4px solid #28a745;
      padding: 14px 18px;
      margin: 12px 0;
      border-radius: 4px;
      color: #155724;
    }
    /* 테이블 */
    table { width: 100%; border-collapse: collapse; font-size: 14px; margin: 12px 0; }
    th {
      background: #f8f9fa;
      font-weight: bold;
      padding: 10px 12px;
      text-align: left;
      border-bottom: 2px solid #dee2e6;
    }
    td { padding: 9px 12px; border-bottom: 1px solid #dee2e6; }
    tr:hover td { background: #fafafa; }
    /* 시각화 */
    .viz-block {
      background: #fafafa;
      border: 1px solid #e9ecef;
      border-radius: 8px;
      padding: 20px;
      margin: 16px 0;
    }
    .viz-title { font-size: 14px; font-weight: bold; color: #555; margin-bottom: 12px; }
    /* 푸터 */
    .report-footer {
      text-align: center;
      padding: 20px 40px;
      border-top: 1px solid #eee;
      color: #999;
      font-size: 12px;
    }
  </style>
</head>
<body>
<div class="container">

  <!-- 헤더 -->
  <div class="report-header">
    <h1> RDQAS 데이터 품질 진단 보고서</h1>
    <div class="report-meta">
      <span>', esc(filename), '</span>
      <span>·</span>
      <span> 분석 목적: ', esc(p_label), '</span>
      <span>·</span>
      <span> ', format(Sys.time(), "%Y-%m-%d %H:%M"), '</span>
    </div>
  </div>

  <div class="body-content">

    <!-- PDF 인쇄 버튼 -->
    <div class="print-btn-wrap">
      <button class="print-btn" onclick="window.print()">
        🖨️ PDF로 저장하기
      </button>
    </div>

    <!-- 1. 요약 -->
    <div class="section">
      <div class="section-title">1. 요약</div>

      <div class="card-grid">
        <div class="metric-card">
          <div class="metric-label">총 행 수</div>
          <div class="metric-value">', format(n_rows, big.mark = ","), '</div>
        </div>
        <div class="metric-card">
          <div class="metric-label">총 열 수</div>
          <div class="metric-value">', n_cols, '</div>
        </div>
        <div class="metric-card">
          <div class="metric-label">결측 셀 수</div>
          <div class="metric-value', ifelse(n_missing_cells > 0, ' warn', ''), '">',
                 format(n_missing_cells, big.mark = ","), '</div>
        </div>
        <div class="metric-card">
          <div class="metric-label">중복 행 수</div>
          <div class="metric-value', ifelse(n_dup_rows > 0, ' warn', ''), '">',
                 n_dup_rows, '</div>
        </div>
      </div>

      <!-- 품질 종합 점수 + 분석 적합성 — 도넛 차트 두 개 나란히 -->
      <div class="donut-pair">

        <!-- 데이터 품질 점수 -->
        <div class="score-donut-wrap" style="border-top-color:', s_color, ';">
          <div class="donut-grade" style="color:', s_color, ';">
            데이터 품질 점수 — ', grade, '
          </div>
          <div class="donut-chart">',
                 make_donut_svg(score, s_color),
                 '</div>
          <div class="donut-legend">
            <span class="legend-dot" style="background:#1D9E75;"></span>90~100 우수<br>
            <span class="legend-dot" style="background:#2196F3;"></span>70~89 양호<br>
            <span class="legend-dot" style="background:#f5821d;"></span>50~69 주의<br>
            <span class="legend-dot" style="background:#E24B4A;"></span>0~49 부적합
          </div>
        </div>

        <!-- 분석 적합성 점수 -->',
                 {
                   s_val   <- suit_score(suitability$status)
                   s_col   <- suit_donut_color(suitability$color)
                   s_label <- suitability$status
                   
                   if (is.na(s_val)) {
                     paste0("
        <div class='score-donut-wrap' style='border-top-color:#aaaaaa;'>
          <div class='donut-grade' style='color:#aaaaaa;'>
            분석 적합성 — ", esc(s_label), "
          </div>
          <div class='donut-chart'>",
                            make_donut_svg(0, "#aaaaaa"),
                            "</div>
          <div class='donut-legend' style='color:#aaaaaa;'>분석 목적을 선택하면 평가됩니다.</div>
        </div>")
                   } else {
                     paste0("
        <div class='score-donut-wrap' style='border-top-color:", s_col, ";'>
          <div class='donut-grade' style='color:", s_col, ";'>
            분석 적합성 — ", esc(s_label), "
          </div>
          <div class='donut-chart'>",
                            make_donut_svg(s_val, s_col),
                            "</div>
          <div class='donut-legend'>
            <span class='legend-dot' style='background:#1D9E75;'></span>적합<br>
            <span class='legend-dot' style='background:#f5821d;'></span>부분 적합<br>
            <span class='legend-dot' style='background:#E24B4A;'></span>부적합
          </div>
        </div>")
                   }
                 },
                 '
      </div><!-- /donut-pair -->
    </div>

    <!-- 2. 데이터 품질 평가 -->
    <div class="section">
      <div class="section-title">2. 데이터 품질 평가</div>

      <!-- 결측치 -->
      <p><strong>🔹 결측치</strong></p>',
                 
                 if (!is.null(missing_df) && nrow(missing_df) > 0) {
                   rows <- paste0(sapply(seq_len(nrow(missing_df)), function(i) {
                     r <- missing_df[i, ]
                     paste0("<tr><td><strong>", esc(r$variable), "</strong></td>",
                            "<td>", r$missing_count, "개</td>",
                            "<td>", r$missing_percent, "%</td></tr>")
                   }), collapse = "\n")
                   paste0("
      <div class='warn-box'>
        <div class='warn-box-title'> 결측치가 발견된 변수</div>
        <table>
          <thead><tr><th>변수명</th><th>결측 수</th><th>결측률</th></tr></thead>
          <tbody>", rows, "</tbody>
        </table>
      </div>")
                 } else {
                   "<div class='ok-box'> 결측치가 없습니다.</div>"
                 },
                 
                 '<!-- 이상치 -->
      <p><strong>🔹 이상치 (IQR 기준, 수치형 변수)</strong></p>',
                 
                 if (!is.null(outlier_df) && nrow(outlier_df) > 0) {
                   rows <- paste0(sapply(seq_len(nrow(outlier_df)), function(i) {
                     r <- outlier_df[i, ]
                     paste0("<tr><td><strong>", esc(r$variable), "</strong></td>",
                            "<td>", r$outlier_count, "개</td>",
                            "<td>", r$outlier_percent, "%</td></tr>")
                   }), collapse = "\n")
                   paste0("
      <div class='warn-box'>
        <div class='warn-box-title'> 이상치가 발견된 변수</div>
        <table>
          <thead><tr><th>변수명</th><th>이상치 수</th><th>비율</th></tr></thead>
          <tbody>", rows, "</tbody>
        </table>
      </div>")
                 } else {
                   "<div class='ok-box'> 이상치가 없습니다.</div>"
                 },
                 
                 '<!-- 형식 일관성 -->
      <p><strong>🔹 형식 일관성 (문자형 변수)</strong></p>',
                 
                 if (!is.null(type_df) && nrow(type_df) > 0) {
                   rows <- paste0(sapply(seq_len(nrow(type_df)), function(i) {
                     r <- type_df[i, ]
                     paste0("<tr><td><strong>", esc(r$variable), "</strong></td>",
                            "<td>", esc(r$issue), "</td></tr>")
                   }), collapse = "\n")
                   paste0("
      <div class='warn-box'>
        <div class='warn-box-title'> 자료형 오류 의심 변수</div>
        <table>
          <thead><tr><th>변수명</th><th>문제 유형</th></tr></thead>
          <tbody>", rows, "</tbody>
        </table>
      </div>")
                 } else {
                   "<div class='ok-box'> 자료형 오류가 없습니다.</div>"
                 },
                 
                 '
    </div>

    <!-- 3. 분석 적합성 평가 -->
    <div class="section">
      <div class="section-title">3. 분석 적합성 평가</div>
      <div style="', suit_bg_color(suitability$color), '
                   padding:18px 22px; border-radius:4px; margin-bottom:16px;">
        <div style="font-size:18px; font-weight:bold; margin-bottom:8px;">
          상태: ', esc(suitability$status), '
        </div>
        <div style="margin:8px 0;">', esc(suitability$message), '</div>',
                 
                 if (recs_html != "") paste0(
                   '<div style="margin-top:12px;"><strong>권장사항:</strong>',
                   recs_html, '</div>'
                 ) else "",
                 
                 '
      </div>
    </div>

    <!-- 4. 컬럼별 품질 상세 -->
    <div class="section">
      <div class="section-title">4. 컬럼별 품질 상세</div>
      <table>
        <thead>
          <tr>
            <th>컬럼명</th>
            <th>타입</th>
            <th>결측률</th>
            <th>중복률</th>
            <th>이상값 수</th>
          </tr>
        </thead>
        <tbody>', col_rows_html, '</tbody>
      </table>
    </div>

    <!-- 5. 시각화 -->
    <div class="section">
      <div class="section-title">5. 시각화</div>

      <div class="viz-block">
        <div class="viz-title"> 결측값 분포</div>',
                 
                 if (missing_plot_tag != "") missing_plot_tag
                 else "<p style='color:#999;'>결측값이 없습니다.</p>",
                 
                 '
      </div>

      <div class="viz-block">
        <div class="viz-title">  수치형 변수 이상값 (박스플롯)</div>',
                 
                 if (boxplot_tag != "") boxplot_tag
                 else "<p style='color:#999;'>수치형 변수가 없습니다.</p>",
                 
                 '
      </div>
    </div>

    <!-- 6. 주요 발견사항 및 권고사항 -->
    <div class="section">
      <div class="section-title">6. 주요 발견사항 및 권고사항</div>',
                 findings_html,
                 '
    </div>

  </div><!-- /body-content -->

  <div class="report-footer">
    본 보고서는 RDQAS 데이터 품질 진단 시스템으로 생성되었습니다.<br>
    최종 분석 결정은 도메인 전문가와 협의하시기 바랍니다.
  </div>

</div><!-- /container -->
</body>
</html>')
  
  writeLines(html, file, useBytes = TRUE)
}