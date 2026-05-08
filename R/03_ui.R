# ============================================================================
# UI 정의
# ============================================================================

ui <- fluidPage(
  # CSS 스타일
  tags$head(
    tags$style(HTML("
      .rdqas-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 30px;
        margin-bottom: 30px;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .rdqas-header h1 {
        margin: 0;
        font-weight: bold;
      }
      
      .rdqas-header p {
        margin: 5px 0 0 0;
        opacity: 0.9;
      }
      
      .section-title {
        font-size: 18px;
        font-weight: bold;
        color: #333;
        margin-top: 30px;
        margin-bottom: 15px;
        border-bottom: 3px solid #667eea;
        padding-bottom: 10px;
      }
      
      .description-box {
        background-color: #f8f9fa;
        border-left: 4px solid #667eea;
        padding: 20px;
        margin: 20px 0;
        border-radius: 4px;
      }
      
      .description-title {
        font-size: 18px;
        font-weight: bold;
        color: #667eea;
        margin: 0 0 10px 0;
      }
      
      .description-content {
        font-size: 15px;
        color: #555;
        line-height: 1.6;
      }
      
      .variable-selector-box {
        background-color: #e3f2fd;
        border-left: 4px solid #2196F3;
        padding: 20px;
        margin: 20px 0;
        border-radius: 4px;
      }
      
      .variable-selector-title {
        font-size: 16px;
        font-weight: bold;
        color: #1565c0;
        margin-bottom: 15px;
      }
      
      .quality-score-display {
        font-size: 48px;
        font-weight: bold;
        text-align: center;
        color: #667eea;
        margin: 20px 0;
      }
      
      .warning-box {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        padding: 15px;
        margin: 10px 0;
        border-radius: 4px;
      }
      
      .warning-box-title {
        font-weight: bold;
        color: #856404;
        margin-bottom: 8px;
      }
      
      .warning-box-content {
        color: #856404;
        margin: 5px 0;
      }
      
      .suitability-box {
        padding: 20px;
        border-radius: 8px;
        margin: 15px 0;
      }
      
      .suitability-box.success {
        background-color: #d4edda;
        border-left: 4px solid #28a745;
        color: #155724;
      }
      
      .suitability-box.warning {
        background-color: #fff3cd;
        border-left: 4px solid #ffc107;
        color: #856404;
      }
      
      .suitability-box.danger {
        background-color: #f8d7da;
        border-left: 4px solid #dc3545;
        color: #721c24;
      }
      
      .suitability-box.info {
        background-color: #d1ecf1;
        border-left: 4px solid #17a2b8;
        color: #0c5460;
      }
      
      .suitability-status {
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 10px;
      }
      
      .suitability-message {
        margin: 10px 0;
      }
      
      .suitability-recommendations {
        margin-top: 15px;
      }
      
      .recommendation-item {
        padding: 8px 0;
        padding-left: 20px;
      }
      
      .recommendation-item:before {
        content: '▸ ';
        margin-left: -15px;
        margin-right: 10px;
      }
      
      .tab-content {
        padding: 20px;
      }
      
      .no-data {
        text-align: center;
        color: #999;
        padding: 40px;
        font-size: 16px;
      }
      
      .btn-primary {
        background-color: #667eea;
        border-color: #667eea;
      }
      
      .btn-primary:hover {
        background-color: #5568d3;
        border-color: #5568d3;
      }
      
      .form-control {
        border-radius: 4px;
        border: 1px solid #ddd;
      }
      
      .form-control:focus {
        border-color: #667eea;
        box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
      }
      
      .table {
        font-size: 14px;
      }
      
      .table thead {
        background-color: #f8f9fa;
      }
      
      .checkbox-group {
        margin: 10px 0;
      }
      
      .checkbox-item {
        padding: 8px;
        margin: 5px 0;
      }
    "))
  ),
  
  # 헤더
  div(class = "rdqas-header",
      h1("📊 RDQAS 데이터 품질 진단 시스템"),
      p("고급 데이터 품질 평가 및 진단 도구")
  ),
  
  fluidRow(
    column(12,
           tabsetPanel(
             id = "main_tabs",
             
             # ========================================================================
             # Tab 1: 데이터 업로드 및 분석 목적 선택
             # ========================================================================
             tabPanel(
               "Step 1: 데이터 업로드",
               icon = icon("upload"),
               
               div(class = "tab-content",
                   
                   # 1. 파일 업로드
                   fluidRow(
                     column(8,
                            div(class = "section-title", "1️⃣ CSV 파일 업로드"),
                            fileInput(
                              "file_upload",
                              "파일을 선택하세요 (.csv 또는 .xlsx)",
                              accept = c(".csv", ".xlsx")
                            ),
                            tags$small("최대 파일 크기: 50MB")
                     )
                   ),
                   
                   # 2. 분석 목적 선택
                   fluidRow(
                     column(8,
                            div(class = "section-title", "2️⃣ 데이터 분석 목적 선택"),
                            selectInput(
                              "analysis_purpose",
                              "분석 목적을 선택하세요:",
                              choices = c(
                                "-- 선택하세요 --" = "",
                                "기초 통계 분석" = "basic_stats",
                                "범주형 변수 빈도 분석" = "visualization",
                                "통계적 검정 / 가설 검정" = "advanced_ml",
                                "상관관계 분석" = "correlation",
                                "회귀분석" = "regression",
                                "분류분석" = "classification",
                                "군집분석" = "cluster",
                                "시계열 분석" = "time_series",
                                "차원 축소 / 변수 구조 탐색" = "dimension",
                                "생존 분석" = "survival"
                              ),
                              selected = ""
                            ),
                            tags$small("선택한 목적에 따라 적합성 평가 기준이 달라집니다.")
                     )
                   ),
                   
                   # 3. 분석 목적 설명
                   fluidRow(
                     column(12,
                            uiOutput("description_output")
                     )
                   ),
                   
                   # 4. 변수 선택 (분석별로 필요한 경우만 표시)
                   # TODO: 각 분석별로 필요한 변수 유형 정의
                   # - correlation: 수치형 변수 선택
                   # - regression: 설명변수(독립변수) + 반응변수(종속변수) 선택
                   # - classification: 설명변수 + 분류변수(범주형 반응변수) 선택
                   # - cluster: 수치형 변수 다중 선택
                   # - etc.
                   fluidRow(
                     column(12,
                            uiOutput("variable_selection_output")
                     )
                   ),
                   
                   # 5. 데이터 미리보기
                   fluidRow(
                     column(12,
                            div(class = "section-title", "3️⃣ 데이터 미리보기"),
                            tableOutput("data_preview")
                     )
                   ),
                   
                   # 6. 진단 시작 버튼
                   fluidRow(
                     column(12,
                            br(),
                            actionButton(
                              "run_diagnosis",
                              "🔍 품질 진단 시작",
                              class = "btn btn-primary btn-lg",
                              icon = icon("play")
                            )
                     )
                   )
               )
             ),
             
             # ========================================================================
             # Tab 2: 진단 결과
             # ========================================================================
             tabPanel(
               "Step 2: 진단 결과",
               icon = icon("chart-bar"),
               
               div(class = "tab-content",
                   uiOutput("diagnosis_results")
               )
             ),
             
             # ========================================================================
             # Tab 3: 분석 실행 (TODO: 추가 예정)
             # ========================================================================
             # TODO: 선택한 분석별 실제 분석 수행 탭
             # - 기초 통계: 평균, 중앙값, 표준편차 등 표시
             # - 시각화: 히스토그램, 막대그래프 등 표시
             # - 가설검정: t-test, ANOVA 결과 표시
             # - 상관관계: 상관계수 행렬 및 히트맵 표시
             # - 회귀분석: 회귀식 및 모형 적합도 표시
             # - etc.
             
             # ========================================================================
             # Tab 4: 보고서 저장
             # ========================================================================
             tabPanel(
               "Step 3: 보고서 저장",
               icon = icon("download"),
               
               div(class = "tab-content",
                   fluidRow(
                     column(8,
                            div(class = "section-title", "HTML 보고서 생성 및 다운로드"),
                            p("진단 결과를 포함한 전체 HTML 보고서를 생성합니다."),
                            br(),
                            downloadButton(
                              "download_report",
                              "📄 HTML 보고서 다운로드",
                              class = "btn btn-primary btn-lg"
                            ),
                            br(), br(),
                            div(class = "warning-box",
                                div(class = "warning-box-title", "ℹ️ 보고서 포함 내용"),
                                div(class = "warning-box-content",
                                    "• 데이터 품질 점수 및 평가",
                                    br(),
                                    "• 결측치, 이상치, 형식 분석",
                                    br(),
                                    "• 적합성 평가 및 권장사항",
                                    br(),
                                    # TODO: 실제 분석 결과 포함 (선택한 분석별로)
                                    "• 선택한 분석의 결과 요약"
                                )
                            )
                     )
                   )
               )
             )
           )
    )
  )
)