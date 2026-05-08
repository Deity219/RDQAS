# ============================================================================
# HTML 보고서 생성 함수
# ============================================================================

generate_html_report <- function(file, data, report, suitability) {
  
  # HTML 특수문자 처리용 함수
  html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }
  
  # HTML 콘텐츠 생성
  html_content <- paste0("
<!DOCTYPE html>
<html>
<head>
  <meta charset='UTF-8'>
  <title>RDQAS 데이터 품질 진단 보고서</title>
  <style>
    body {
      font-family: 'Arial', sans-serif;
      line-height: 1.6;
      color: #333;
      margin: 0;
      padding: 20px;
      background-color: #f5f5f5;
    }
    .container {
      max-width: 900px;
      margin: 0 auto;
      background-color: white;
      padding: 40px;
      border-radius: 8px;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    }
    .header {
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      padding: 30px;
      margin: -40px -40px 30px -40px;
      border-radius: 8px 8px 0 0;
      text-align: center;
    }
    .header h1 {
      margin: 0;
      font-size: 32px;
    }
    .header p {
      margin: 10px 0 0 0;
      opacity: 0.9;
    }
    .section {
      margin: 30px 0;
    }
    .section-title {
      font-size: 20px;
      font-weight: bold;
      color: #333;
      border-bottom: 3px solid #667eea;
      padding-bottom: 10px;
      margin-bottom: 20px;
    }
    .score-display {
      font-size: 48px;
      font-weight: bold;
      color: #667eea;
      text-align: center;
      padding: 30px;
      background-color: #f8f9fa;
      border-radius: 8px;
      margin: 20px 0;
    }
    .warning-box {
      background-color: #fff3cd;
      border-left: 4px solid #ffc107;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
    }
    .warning-title {
      font-weight: bold;
      color: #856404;
      margin-bottom: 8px;
    }
    .warning-content {
      color: #856404;
      margin: 5px 0;
    }
    .success-box {
      background-color: #d4edda;
      border-left: 4px solid #28a745;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
      color: #155724;
    }
    table {
      width: 100%;
      border-collapse: collapse;
      margin: 15px 0;
    }
    th, td {
      padding: 12px;
      text-align: left;
      border-bottom: 1px solid #ddd;
    }
    th {
      background-color: #f8f9fa;
      font-weight: bold;
    }
    tr:hover {
      background-color: #f5f5f5;
    }
    .suitability-status {
      font-size: 18px;
      font-weight: bold;
      padding: 10px;
      margin: 10px 0;
      border-radius: 4px;
    }
    .status-success {
      background-color: #d4edda;
      color: #155724;
    }
    .status-warning {
      background-color: #fff3cd;
      color: #856404;
    }
    .status-danger {
      background-color: #f8d7da;
      color: #721c24;
    }
    .status-info {
      background-color: #d1ecf1;
      color: #0c5460;
    }
    .status-secondary {
      background-color: #e2e3e5;
      color: #383d41;
    }
    .recommendation {
      margin: 10px 0;
      padding-left: 20px;
    }
    .recommendation:before {
      content: '▸ ';
      margin-left: -15px;
      margin-right: 10px;
      color: #667eea;
    }
    .info-box {
      background-color: #e7f3ff;
      border-left: 4px solid #2196F3;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
      color: #1565c0;
    }
    .footer {
      text-align: center;
      margin-top: 40px;
      padding-top: 20px;
      border-top: 1px solid #ddd;
      color: #666;
      font-size: 12px;
    }
  </style>
</head>

<body>
  <div class='container'>
    <div class='header'>
      <h1>📊 RDQAS 데이터 품질 진단 보고서</h1>
      <p>생성일: ", Sys.Date(), "</p>
    </div>
    
    <div class='section'>
      <div class='section-title'>📈 데이터 품질 점수</div>
      <div class='score-display'>", report$score, "/100</div>
      <div class='info-box'>
        <strong>평가 기준:</strong> 90~100점 (우수) | 70~89점 (양호) | 50~69점 (주의) | 0~49점 (부진)
      </div>
    </div>
    
    <div class='section'>
      <div class='section-title'>⚠️ 주요 경고</div>"
  )
  
  # 결측치 결과
  if (!is.null(report$missing) && nrow(report$missing) > 0) {
    html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 결측치 발견</div>")
    
    for (i in 1:nrow(report$missing)) {
      row <- report$missing[i, ]
      html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", html_escape(row$variable), "</strong>: ",
                             row$missing_percent, "% (", row$missing_count, "개)
        </div>")
    }
    
    html_content <- paste0(html_content, "
      </div>")
  } else {
    html_content <- paste0(html_content, "
      <div class='success-box'>✅ 결측치가 없습니다.</div>")
  }
  
  # 이상치 결과
  if (!is.null(report$outliers) && nrow(report$outliers) > 0) {
    html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 이상치 발견</div>")
    
    for (i in 1:nrow(report$outliers)) {
      row <- report$outliers[i, ]
      html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", html_escape(row$variable), "</strong>: ",
                             row$outlier_percent, "% (", row$outlier_count, "개)
        </div>")
    }
    
    html_content <- paste0(html_content, "
      </div>")
  } else {
    html_content <- paste0(html_content, "
      <div class='success-box'>✅ 이상치가 없습니다.</div>")
  }
  
  # 형식 일관성 결과
  if (!is.null(report$format) && nrow(report$format) > 0) {
    html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 형식 일관성 문제</div>")
    
    for (i in 1:nrow(report$format)) {
      row <- report$format[i, ]
      html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", html_escape(row$variable), "</strong>: ",
                             html_escape(row$issue), "
        </div>")
    }
    
    html_content <- paste0(html_content, "
      </div>")
  } else {
    html_content <- paste0(html_content, "
      <div class='success-box'>✅ 형식 일관성이 좋습니다.</div>")
  }
  
  # 적합성 평가
  html_content <- paste0(html_content, "
    </div>
    
    <div class='section'>
      <div class='section-title'>📋 적합성 평가</div>
      <div class='suitability-status status-", suitability$color, "'>
        상태: ", html_escape(suitability$status), "
      </div>
      <p>", html_escape(suitability$message), "</p>")
  
  # 권장사항
  if (!is.null(suitability$recommendations) && length(suitability$recommendations) > 0) {
    html_content <- paste0(html_content, "<p><strong>권장사항:</strong></p>")
    
    for (rec in suitability$recommendations) {
      html_content <- paste0(html_content, "
      <div class='recommendation'>", html_escape(rec), "</div>")
    }
  }
  
  # 데이터셋 정보
  html_content <- paste0(html_content, "
    </div>
    
    <div class='section'>
      <div class='section-title'>📊 데이터셋 정보</div>
      <table>
        <tr>
          <th>항목</th>
          <th>값</th>
        </tr>
        <tr>
          <td>행(Row) 수</td>
          <td>", nrow(data), "</td>
        </tr>
        <tr>
          <td>열(Column) 수</td>
          <td>", ncol(data), "</td>
        </tr>
        <tr>
          <td>수치형 변수</td>
          <td>", sum(sapply(data, is.numeric)), "</td>
        </tr>
        <tr>
          <td>문자형 변수</td>
          <td>", sum(sapply(data, is.character)), "</td>
        </tr>
        <tr>
          <td>날짜형 변수</td>
          <td>", sum(sapply(data, function(x) inherits(x, 'Date'))), "</td>
        </tr>
      </table>
    </div>
    
    <!-- TODO: 선택한 분석 결과 섹션 추가 -->
    <!-- 여기에 실제 분석 결과(기초통계, 시각화, 통계검정 결과 등)를 포함시키기 -->
    
    <div class='footer'>
      <p>RDQAS 데이터 품질 진단 시스템에서 생성되었습니다.</p>
      <p>이 보고서는 참고용입니다. 최종 분석 결정은 도메인 전문가와 협의하시기 바랍니다.</p>
    </div>
  </div>
</body>
</html>")
  
  writeLines(html_content, file, useBytes = TRUE)
}