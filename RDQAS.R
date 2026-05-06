library(shiny)
library(shinyBS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(rmarkdown)
library(readxl)

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
      has_case_mix <- any(data[[col]] != tolower(data[[col]], na.rm = TRUE), na.rm = TRUE) &
                      any(data[[col]] != toupper(data[[col]], na.rm = TRUE), na.rm = TRUE)
      
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

# ============================================================================
# Server 로직
# ============================================================================

server <- function(input, output, session) {
  
  # 반응형 데이터 저장소
  data_store <- reactiveValues(
    original_data = NULL,
    quality_report = NULL,
    suitability = NULL,
    selected_variables = NULL
  )
  
  # ============================================================================
  # 분석 정보 정의 (각 분석별 설명 및 변수 요구사항)
  # ============================================================================
  
  analysis_info <- list(
    basic_stats = list(
      icon = "📈",
      title = "기초 통계 분석",
      description = "데이터의 기본 요약값을 확인하고 싶어요",
      explain = "선택한 변수들의 평균, 중앙값, 표준편차, 최소값, 최대값 등의 기본 통계량을 계산하고 분포를 시각화합니다.",
      keywords = c("평균", "표준편차", "분포", "요약통계"),
      requires_variable_selection = FALSE
    ),
    visualization = list(
      icon = "📊",
      title = "범주형 변수 빈도 분석",
      description = "범주별 개수와 비율을 확인하고 싶어요",
      explain = "성별, 학력, 지역 등 범주형 변수의 각 카테고리별 빈도와 상대 빈도를 계산합니다.",
      keywords = c("빈도", "비율", "카테고리", "분할표"),
      requires_variable_selection = FALSE
    ),
    advanced_ml = list(
      icon = "🔬",
      title = "통계적 검정 / 가설 검정",
      description = "집단 간 차이 검정",
      explain = "t-test, ANOVA 등 수행",
      keywords = c("p-value", "검정"),
      requires_variable_selection = TRUE,
      requires_test_selection = TRUE
    ),
    correlation = list(
      icon = "🔗",
      title = "상관관계 분석",
      description = "변수들 사이의 관련성을 확인하고 싶어요",
      explain = "피어슨 상관계수, 스피어만 상관계수를 계산하여 연속형 변수 간의 선형 관계를 측정합니다.",
      keywords = c("상관계수", "관계성", "산점도", "히트맵"),
      requires_variable_selection = TRUE,
      variable_types = list(
        multiple = "numeric"
      )
    ),
    regression = list(
      icon = "📐",
      title = "회귀분석",
      description = "수치형 결과값을 설명하거나 예측하고 싶어요",
      explain = "단순선형회귀, 다중회귀분석을 수행하여 독립변수가 종속변수에 미치는 영향을 분석합니다.",
      keywords = c("선형회귀", "예측", "영향도", "적합도"),
      requires_variable_selection = TRUE,
      variable_types = list(
        explanatory = "numeric",
        response = "numeric"
      )
    ),
    classification = list(
      icon = "🎯",
      title = "분류분석",
      description = "범주형 결과를 예측하거나 분류하고 싶어요",
      explain = "로지스틱 회귀, 의사결정 나무 등의 알고리즘을 사용하여 데이터를 범주에 자동 분류합니다.",
      keywords = c("분류", "예측", "정확도", "머신러닝"),
      requires_variable_selection = TRUE,
      variable_types = list(
        explanatory = "numeric_or_character",
        response = "character"
      )
    ),
    cluster = list(
      icon = "🎲",
      title = "군집분석",
      description = "비슷한 특성을 가진 데이터끼리 그룹으로 나누고 싶어요",
      explain = "K-means, 계층적 군집분석 등을 사용하여 데이터를 자동으로 그룹화합니다.",
      keywords = c("그룹화", "비지도학습", "세분화", "클러스터"),
      requires_variable_selection = TRUE,
      variable_types = list(
        multiple = "numeric"
      )
    ),
    time_series = list(
      icon = "📈",
      title = "시계열 분석",
      description = "시간에 따른 변화나 추세를 확인하고 싶어요",
      explain = "추세, 계절성, 주기성을 분석하고 ARIMA 등의 기법으로 미래 값을 예측합니다.",
      keywords = c("추세", "계절성", "예측", "시간"),
      requires_variable_selection = TRUE,
      variable_types = list(
        time = "Date",
        value = "numeric"
      )
    ),
    dimension = list(
      icon = "🔍",
      title = "차원 축소 / 변수 구조 탐색",
      description = "여러 변수를 요약해 핵심 구조를 파악하고 싶어요",
      explain = "주성분분석(PCA), 요인분석 등을 통해 많은 변수를 소수의 주요 성분으로 축약합니다.",
      keywords = c("PCA", "축약", "구조", "시각화"),
      requires_variable_selection = TRUE,
      variable_types = list(
        multiple = "numeric"
      )
    ),
    survival = list(
      icon = "⏱️",
      title = "생존 분석",
      description = "사건이 발생하기까지의 시간과 관련 요인을 분석하고 싶어요",
      explain = "Kaplan-Meier 생존함수, Cox 비례위험 모형 등을 사용하여 사건 발생 시간을 분석합니다.",
      keywords = c("생존율", "위험도", "시간", "의료통계"),
      requires_variable_selection = TRUE,
      variable_types = list(
        time = "numeric",
        event = "numeric_or_character",
        covariate = "numeric_or_character"
      )
    )
  )
  
# ============================================================================
#   검정 종류 선택 
# ============================================================================
  
  output$variable_selection_output <- renderUI({
    
    if (input$analysis_purpose != "advanced_ml") return(NULL)
    
    fluidRow(
      column(12,
             div(class = "variable-selector-box",
                 
                 selectInput(
                   "test_type",
                   "검정 유형:",
                   choices = c(
                     "-- 선택하세요 --" = "",
                     "평균 검정" = "mean_test",
                     "카이제곱 검정" = "chi_square",
                     "상관 검정" = "correlation_test"
                   )
                 ),
                 
                 uiOutput("test_variable_selection_output")
             )
      )
    )
  })
  
  # ============================================================================
  #   변수 선택 
  # ============================================================================ 
  
  output$test_variable_selection_output <- renderUI({
    
    req(data_store$original_data)
    req(input$test_type)
    
    numeric_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.numeric)
    ]
    
    character_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.character)
    ]
    
    if (input$test_type == "mean_test") {
      fluidRow(
        column(6,
               radioButtons("group_var", "그룹 변수", character_vars)
        ),
        column(6,
               radioButtons("value_var", "값 변수", numeric_vars)
        )
      )
    }
  })
  
  
  # ============================================================================
  # 1. 파일 업로드 처리
  # ============================================================================
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    file_path <- input$file_upload$datapath
    file_ext <- tolower(tools::file_ext(input$file_upload$name))
    
    tryCatch({
      if (file_ext == "csv") {
        data_store$original_data <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if (file_ext == "xlsx") {
        data_store$original_data <- as.data.frame(read_excel(file_path))
      }
      
      showNotification(
        "파일이 성공적으로 로드되었습니다.",
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("파일 로드 오류:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # ============================================================================
  # 2. 분석 목적 설명 출력
  # ============================================================================
  
  output$description_output <- renderUI({
    if (input$analysis_purpose == "" || is.null(input$analysis_purpose)) {
      return(
        div("분석 목적을 선택하면 설명이 여기에 표시됩니다.")
      )
    }
    
    info <- analysis_info[[input$analysis_purpose]]
    
    div(class = "description-box",
      div(class = "description-title",
        info$icon, " ", info$title
      ),
      div(class = "description-content",
        p(strong("선택한 분석:"), info$description),
        p(info$explain),
        p(
          strong("주요 개념:"),
          paste(info$keywords, collapse = " • ")
        )
      )
    )
  })
  
  # ============================================================================
  # 3. 변수 선택 UI 출력 (분석별로 필요한 경우만)
  # ============================================================================
  
  output$variable_selection_output <- renderUI({
    # 파일이 업로드되지 않았으면 표시 안함
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    
    # 변수 선택이 필요하지 않은 분석이면 표시 안함
    if (input$analysis_purpose == "" || is.null(input$analysis_purpose)) {
      return(NULL)
    }
    
    info <- analysis_info[[input$analysis_purpose]]
    
    # 변수 선택이 필요 없는 분석
    if (!info$requires_variable_selection) {
      return(NULL)
    }
    
    # 변수 유형별 분류
    numeric_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.numeric)
    ]
    character_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.character)
    ]
    date_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, function(x) inherits(x, "Date"))
    ]
    
    # 분석별로 다른 UI 구성
    variable_ui <- switch(input$analysis_purpose,
      # ============================================================
      # 통계적 검정: 검정 유형 선택
      # ============================================================
      advanced_ml = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "🔬 통계적 검정 설정"),
              
              # 검정 유형 선택
              fluidRow(
                column(12,
                  p(strong("1️⃣ 검정 유형 선택")),
                  p("데이터 특성에 맞는 검정을 선택하세요."),
                  selectInput(
                    "test_type",
                    "검정 유형:",
                    choices = list(
                      "-- 선택하세요 --" = "",
                      "📊 평균 검정 (그룹 간 평균 비교)" = "mean_test",
                      "✅ 범주형 독립성 검정 (범주형 변수 관계)" = "chi_square",
                      "🔗 상관 검정 (연속형 변수 관계)" = "correlation_test"
                    ),
                    selected = ""
                  ),
                  uiOutput("test_description_output")
                )
              ),
              
              hr(),
              
              # 변수 선택 (검정 유형에 따라 다름)
              fluidRow(
                column(12,
                  p(strong("2️⃣ 변수 선택")),
                  p("선택한 검정에 필요한 변수를 선택하세요."),
                  uiOutput("test_variable_selection_output")
                )
              )
            )
          )
        )
      },
      
      # ============================================================
      # 상관관계 분석: 여러 수치형 변수 선택
      # ============================================================
      correlation = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "📊 상관관계 분석할 변수 선택 (수치형)"),
              p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
              checkboxGroupInput(
                "corr_variables",
                "변수 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          )
        )
      },
      
      # ============================================================
      # 회귀분석: 설명변수(독립변수)와 반응변수(종속변수) 선택
      # ============================================================
      regression = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "📐 회귀분석 변수 선택"),
              
              # 설명변수 (독립변수) 선택
              fluidRow(
                column(6,
                  p(strong("설명변수 (독립변수) - 수치형")),
                  p("하나 이상 선택해야 합니다."),
                  checkboxGroupInput(
                    "reg_explanatory",
                    "설명변수:",
                    choices = numeric_vars,
                    selected = NULL
                  )
                ),
                
                # 반응변수 (종속변수) 선택
                column(6,
                  p(strong("반응변수 (종속변수) - 수치형")),
                  p("정확히 1개 선택해야 합니다."),
                  radioButtons(
                    "reg_response",
                    "반응변수:",
                    choices = numeric_vars,
                    selected = NULL
                  )
                )
              )
            )
          )
        )
      },
      
      # ============================================================
      # 분류분석: 설명변수와 분류변수 선택
      # ============================================================
      classification = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "🎯 분류분석 변수 선택"),
              
              # 설명변수
              fluidRow(
                column(6,
                  p(strong("설명변수 (독립변수)")),
                  p("수치형 또는 범주형, 하나 이상 선택"),
                  checkboxGroupInput(
                    "class_explanatory",
                    "설명변수:",
                    choices = c(numeric_vars, character_vars),
                    selected = NULL
                  )
                ),
                
                # 분류변수 (종속변수)
                column(6,
                  p(strong("분류변수 (종속변수)")),
                  p("범주형 변수, 정확히 1개 선택"),
                  radioButtons(
                    "class_response",
                    "분류변수:",
                    choices = character_vars,
                    selected = NULL
                  )
                )
              )
            )
          )
        )
      },
      
      # ============================================================
      # 군집분석: 여러 수치형 변수 선택
      # ============================================================
      cluster = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "🎲 군집분석할 변수 선택 (수치형)"),
              p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
              checkboxGroupInput(
                "cluster_variables",
                "변수 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          )
        )
      },
      
      # ============================================================
      # 시계열 분석: 시간변수와 값 변수 선택
      # ============================================================
      time_series = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "📈 시계열 분석 변수 선택"),
              
              fluidRow(
                column(6,
                  p(strong("시간변수")),
                  p("Date 또는 numeric (정렬된 시간 순서), 1개 선택"),
                  radioButtons(
                    "ts_time",
                    "시간변수:",
                    choices = c(date_vars, numeric_vars),
                    selected = NULL
                  )
                ),
                
                column(6,
                  p(strong("값 변수 (수치형)")),
                  p("분석할 수치형 변수, 하나 이상 선택"),
                  checkboxGroupInput(
                    "ts_value",
                    "값 변수:",
                    choices = numeric_vars,
                    selected = NULL
                  )
                )
              )
            )
          )
        )
      },
      
      # ============================================================
      # 차원 축소: 여러 수치형 변수 선택
      # ============================================================
      dimension = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "🔍 차원 축소 변수 선택 (수치형)"),
              p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
              checkboxGroupInput(
                "dim_variables",
                "변수 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          )
        )
      },
      
      # ============================================================
      # 생존 분석: 시간, 사건, 공변량 선택
      # ============================================================
      survival = {
        fluidRow(
          column(12,
            div(class = "variable-selector-box",
              div(class = "variable-selector-title", "⏱️ 생존 분석 변수 선택"),
              
              fluidRow(
                column(4,
                  p(strong("시간변수")),
                  p("follow-up time (수치형)"),
                  radioButtons(
                    "surv_time",
                    "시간변수:",
                    choices = numeric_vars,
                    selected = NULL
                  )
                ),
                
                column(4,
                  p(strong("사건변수")),
                  p("사건 여부 (0/1 또는 No/Yes)"),
                  radioButtons(
                    "surv_event",
                    "사건변수:",
                    choices = c(numeric_vars, character_vars),
                    selected = NULL
                  )
                ),
                
                column(4,
                  p(strong("공변량 (선택)")),
                  p("분석에 포함할 변수"),
                  checkboxGroupInput(
                    "surv_covariate",
                    "공변량:",
                    choices = c(numeric_vars, character_vars),
                    selected = NULL
                  )
                )
              )
            )
          )
        )
      },
      
      # ============================================================
      # 기타 분석 (TODO: 필요시 추가)
      # ============================================================
      NULL
    )
    
    return(variable_ui)
  })
  
  # ============================================================================
  # 4. 데이터 미리보기
  # ============================================================================
  
  output$data_preview <- renderTable({
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    head(data_store$original_data, 10)
  }, options = list(scrollX = TRUE))
  
  # ============================================================================
  # 5. 품질 진단 실행
  # ============================================================================
  
  observeEvent(input$run_diagnosis, {
    req(data_store$original_data)
    
    # 진단 수행
    data_store$quality_report <- assess_data_quality(data_store$original_data)
    data_store$suitability <- evaluate_suitability(
      data_store$quality_report,
      input$analysis_purpose
    )
    
    # Step 2 탭으로 이동
    updateTabsetPanel(session, "main_tabs", selected = "Step 2: 진단 결과")
    
    showNotification(
      "진단이 완료되었습니다!",
      type = "message",
      duration = 3
    )
  })
  
  # ============================================================================
  # 6. 진단 결과 출력
  # ============================================================================
  
  output$diagnosis_results <- renderUI({
    if (is.null(data_store$quality_report)) {
      return(div(class = "no-data",
        "아직 진단을 실행하지 않았습니다.",
        br(),
        "Step 1에서 파일을 업로드하고 진단을 시작하세요."
      ))
    }
    
    report <- data_store$quality_report
    suitability <- data_store$suitability
    
    list(
      # 품질 점수
      fluidRow(
        column(12,
          div(class = "section-title", "📈 데이터 품질 점수")
        )
      ),
      fluidRow(
        column(4,
          div(class = "quality-score-display",
            report$score, "점"
          )
        ),
        column(8,
          div(
            p("총 100점 중 획득한 점수입니다."),
            p(
              strong("평가 기준: "),
              "90~100점 (우수), 70~89점 (양호), 50~69점 (주의), 0~49점 (부진)"
            )
          )
        )
      ),
      
      # 주요 경고: 결측치
      fluidRow(
        column(12,
          div(class = "section-title", "⚠️ 주요 경고")
        )
      ),
      if (nrow(report$missing) > 0) {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-title", "❌ 결측치 발견"),
              lapply(1:nrow(report$missing), function(i) {
                row <- report$missing[i, ]
                div(class = "warning-box-content",
                  HTML(paste0(
                    "<strong>", row$variable, "</strong> 변수에 ",
                    row$missing_percent, "% (", row$missing_count, "개)의 결측치가 존재합니다."
                  ))
                )
              })
            )
          )
        )
      } else {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-content", "✅ 결측치가 없습니다.")
            )
          )
        )
      },
      
      # 주요 경고: 이상치
      if (nrow(report$outliers) > 0) {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-title", "❌ 이상치 발견"),
              lapply(1:nrow(report$outliers), function(i) {
                row <- report$outliers[i, ]
                div(class = "warning-box-content",
                  HTML(paste0(
                    "<strong>", row$variable, "</strong> 변수에서 ",
                    row$outlier_percent, "% (", row$outlier_count, "개)의 이상치가 발견되었습니다."
                  ))
                )
              })
            )
          )
        )
      } else {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-content", "✅ 이상치가 없습니다.")
            )
          )
        )
      },
      
      # 주요 경고: 형식 일관성
      if (nrow(report$format) > 0) {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-title", "❌ 값 형식 일관성 문제"),
              lapply(1:nrow(report$format), function(i) {
                row <- report$format[i, ]
                div(class = "warning-box-content",
                  HTML(paste0(
                    "<strong>", row$variable, "</strong> 변수: ", row$issue
                  ))
                )
              })
            )
          )
        )
      } else {
        fluidRow(
          column(12,
            div(class = "warning-box",
              div(class = "warning-box-content", "✅ 형식 일관성이 좋습니다.")
            )
          )
        )
      },
      
      # 적합성 평가
      fluidRow(
        column(12,
          div(class = "section-title", "📋 적합성 평가")
        )
      ),
      fluidRow(
        column(8,
          div(class = paste("suitability-box", suitability$color),
            div(class = "suitability-status",
              "상태: ", suitability$status
            ),
            div(class = "suitability-message",
              suitability$message
            ),
            if (length(suitability$recommendations) > 0) {
              div(class = "suitability-recommendations",
                strong("권장사항:"),
                div(class = "recommendation-item",
                  lapply(suitability$recommendations, function(rec) {
                    div(rec)
                  })
                )
              )
            }
          )
        )
      ),
      
      # 상세 분석 표
      fluidRow(
        column(12,
          div(class = "section-title", "📊 데이터셋 정보"),
          tableOutput("dataset_info")
        )
      )
    )
  })
  
  # ============================================================================
  # 7. 데이터셋 정보 표
  # ============================================================================
  
  output$dataset_info <- renderTable({
    if (is.null(data_store$original_data)) return(NULL)
    
    data.frame(
      항목 = c("행(Row) 수", "열(Column) 수", "수치형 변수", "문자형 변수", "날짜형 변수"),
      값 = c(
        nrow(data_store$original_data),
        ncol(data_store$original_data),
        sum(sapply(data_store$original_data, is.numeric)),
        sum(sapply(data_store$original_data, is.character)),
        sum(sapply(data_store$original_data, function(x) inherits(x, "Date")))
      )
    )
  })
  
  # ============================================================================
  # 검정 유형별 설명 출력
  # ============================================================================
  output$test_description_output <- renderUI({
    if (input$test_type == "" || is.null(input$test_type)) {
      return(div(style = "color: #999; margin-top: 10px;",
        "검정 유형을 선택하면 설명이 여기 나타납니다."
      ))
    }
    
    test_descriptions <- list(
      mean_test = list(
        description = "두 개 이상의 그룹 간 평균의 차이가 통계적으로 유의미한지 검정합니다.",
        conditions = "• 두 그룹: t-검정 사용",
        conditions2 = "• 세 개 이상 그룹: ANOVA 사용",
        use_case = "예) 남녀 학생의 수학점수 비교 또는 A, B, C 처리 방법의 효과 비교"
      ),
      chi_square = list(
        description = "두 개의 범주형 변수 간의 독립성을 검정합니다 (관계가 있는지 없는지).",
        conditions = "• 범주형 변수 2개 필요",
        conditions2 = "• 기대도수 ≥ 5 (각 셀)",
        use_case = "예) 성별과 선호도의 관계, 지역과 구매 의사의 관계"
      ),
      correlation_test = list(
        description = "두 개의 연속형(수치형) 변수 간의 선형 관계가 통계적으로 유의미한지 검정합니다.",
        conditions = "• 연속형 변수 2개 필요",
        conditions2 = "• Pearson 상관계수 또는 Spearman 순위상관계수 사용",
        use_case = "예) 키와 몸무게의 관계, 공부시간과 시험점수의 관계"
      )
    )
    
    if (!is.null(test_descriptions[[input$test_type]])) {
      desc <- test_descriptions[[input$test_type]]
      div(
        style = "background-color: #e3f2fd; border-left: 4px solid #2196F3; padding: 15px; margin-top: 10px; border-radius: 4px;",
        p(strong("📖 설명:"), desc$description),
        p(strong("방법:"), br(), desc$conditions),
        p(desc$conditions2),
        p(strong("사용 예:"), desc$use_case)
      )
    }
  })
  
  # ============================================================================
  # 검정 유형별 변수 선택 UI
  # ============================================================================
  output$test_variable_selection_output <- renderUI({
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    
    if (input$test_type == "" || is.null(input$test_type)) {
      return(NULL)
    }
    
    numeric_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.numeric)
    ]
    character_vars <- names(data_store$original_data)[
      sapply(data_store$original_data, is.character)
    ]
    
    switch(input$test_type,
      # 평균 검정
      mean_test = {
        fluidRow(
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 그룹 변수 (설명변수)")),
              p("비교하려는 그룹을 나타내는 범주형 변수"),
              p("예) 성별 (남/여), 처리방법 (A/B/C)"),
              radioButtons(
                "mean_group",
                "그룹변수 선택:",
                choices = character_vars,
                selected = NULL
              )
            )
          ),
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 값 변수 (반응변수)")),
              p("비교하려는 측정값으로 수치형 변수"),
              p("예) 수학점수, 몸무게, 혈압"),
              radioButtons(
                "mean_value",
                "값변수 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          )
        )
      },
      # 범주형 독립성 검정
      chi_square = {
        fluidRow(
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 변수 1 (범주형)")),
              p("첫 번째 범주형 변수"),
              p("예) 성별, 지역, 학년"),
              radioButtons(
                "chi_var1",
                "변수1 선택:",
                choices = character_vars,
                selected = NULL
              )
            )
          ),
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 변수 2 (범주형)")),
              p("두 번째 범주형 변수"),
              p("예) 선호도, 구매의사, 만족도"),
              radioButtons(
                "chi_var2",
                "변수2 선택:",
                choices = character_vars,
                selected = NULL
              )
            )
          )
        )
      },
      # 상관 검정
      correlation_test = {
        fluidRow(
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 변수 1 (연속형)")),
              p("첫 번째 수치형 변수"),
              p("예) 키, 공부시간, 광고비"),
              radioButtons(
                "corr_var1",
                "변수1 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          ),
          column(6,
            div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
              p(strong("📌 변수 2 (연속형)")),
              p("두 번째 수치형 변수"),
              p("예) 몸무게, 시험점수, 판매액"),
              radioButtons(
                "corr_var2",
                "변수2 선택:",
                choices = numeric_vars,
                selected = NULL
              )
            )
          )
        )
      },
      NULL
    )
  })
  
  # ============================================================================
  # 8. HTML 보고서 생성 및 다운로드
  # ============================================================================
  # TODO: 선택한 분석 결과를 보고서에 포함시키기
  # - 선택한 변수들이 어떤 것인지 표시
  # - 분석 결과 요약 포함
  # - 실제 분석 수행 후 결과 통계량 포함
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("RDQAS_Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      report <- data_store$quality_report
      suitability <- data_store$suitability
      data <- data_store$original_data
      
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
      
      if (nrow(report$missing) > 0) {
        html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 결측치 발견</div>")
        for (i in 1:nrow(report$missing)) {
          row <- report$missing[i, ]
          html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", row$variable, "</strong>: ", row$missing_percent, "% (", row$missing_count, "개)
        </div>")
        }
        html_content <- paste0(html_content, "
      </div>")
      } else {
        html_content <- paste0(html_content, "
      <div class='success-box'>✅ 결측치가 없습니다.</div>")
      }
      
      if (nrow(report$outliers) > 0) {
        html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 이상치 발견</div>")
        for (i in 1:nrow(report$outliers)) {
          row <- report$outliers[i, ]
          html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", row$variable, "</strong>: ", row$outlier_percent, "% (", row$outlier_count, "개)
        </div>")
        }
        html_content <- paste0(html_content, "
      </div>")
      } else {
        html_content <- paste0(html_content, "
      <div class='success-box'>✅ 이상치가 없습니다.</div>")
      }
      
      if (nrow(report$format) > 0) {
        html_content <- paste0(html_content, "
      <div class='warning-box'>
        <div class='warning-title'>❌ 형식 일관성 문제</div>")
        for (i in 1:nrow(report$format)) {
          row <- report$format[i, ]
          html_content <- paste0(html_content, "
        <div class='warning-content'>
          <strong>", row$variable, "</strong>: ", row$issue, "
        </div>")
        }
        html_content <- paste0(html_content, "
      </div>")
      } else {
        html_content <- paste0(html_content, "
      <div class='success-box'>✅ 형식 일관성이 좋습니다.</div>")
      }
      
      html_content <- paste0(html_content, "
    </div>
    
    <div class='section'>
      <div class='section-title'>📋 적합성 평가</div>
      <div class='suitability-status status-", suitability$color, "'>
        상태: ", suitability$status, "
      </div>
      <p>", suitability$message, "</p>")
      
      if (length(suitability$recommendations) > 0) {
        html_content <- paste0(html_content, "<p><strong>권장사항:</strong></p>")
        for (rec in suitability$recommendations) {
          html_content <- paste0(html_content, "
      <div class='recommendation'>", rec, "</div>")
        }
      }
      
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
          <td>", sum(sapply(data, function(x) inherits(x, "Date"))), "</td>
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
      
      writeLines(html_content, file)
    }
  )
}

# ============================================================================
# 앱 실행
# ============================================================================
shinyApp(ui, server)
