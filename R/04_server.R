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
  
  numeric_vars_current <- reactive({
    req(data_store$original_data)
    names(data_store$original_data)[sapply(data_store$original_data, is.numeric)]
  })
  character_vars_current <- reactive({
    req(data_store$original_data)
    names(data_store$original_data)[sapply(data_store$original_data, is.character)]
  })
  
  # ============================================================================
  # 분석 정보 정의
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
      explain = "t-test, ANOVA, 카이제곱 검정, 상관 검정 등을 수행합니다.",
      keywords = c("p-value", "검정", "가설검정"),
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
      } else {
        stop("지원하지 않는 파일 형식입니다. CSV 또는 XLSX 파일을 업로드하세요.")
      }
      
      data_store$quality_report <- NULL
      data_store$suitability_result <- NULL
      data_store$selected_variables <- NULL
      
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
    
    if (is.null(info)) {
      return(
        div(class = "warning-box",
            div(class = "warning-box-content", "지원하지 않는 분석 목적입니다.")
        )
      )
    }
    
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
  # 3. 변수 선택 UI 출력
  # ============================================================================
  
  output$variable_selection_output <- renderUI({
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    
    if (input$analysis_purpose == "" || is.null(input$analysis_purpose)) {
      return(NULL)
    }
    
    info <- analysis_info[[input$analysis_purpose]]
    
    if (is.null(info)) {
      return(NULL)
    }
    
    if (!info$requires_variable_selection) {
      return(NULL)
    }
    
    type_result <- detect_all_variable_types(data_store$original_data)
    
    numeric_vars <- type_result$variable[
      type_result$variable_type == "수치형 변수"
    ]
    
    character_vars <- type_result$variable[
      type_result$variable_type %in% c("범주형 변수", "문자형 변수")
    ]
    
    date_vars <- type_result$variable[
      type_result$variable_type == "날짜형 변수" |
        (!is.null(type_result$is_time_candidate) & type_result$is_time_candidate)
    ]
    
    # 빈 경우 안내 메시지용 더미값
    if (length(numeric_vars) == 0)   numeric_vars   <- c("수치형 변수 없음" = "")
    if (length(character_vars) == 0) character_vars <- c("범주형 변수 없음" = "")
    if (length(date_vars) == 0)      date_vars      <- c("날짜형 변수 없음" = "")
    
    variable_ui <- switch(
      input$analysis_purpose,
      
      advanced_ml = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "🔬 통계적 검정 설정"),
                     
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
      
      correlation = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "📊 상관관계 분석할 변수 선택 (수치형)"),
                     p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
                     selectizeInput(
                       "corr_variables",
                       "변수 선택 (검색 가능):",
                       choices = numeric_vars,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "변수명을 입력하거나 선택하세요")
                     ),
                     fluidRow(
                       column(6, actionButton("corr_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                       column(6, actionButton("corr_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                     )
                 )
          )
        )
      },
      
      regression = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "📐 회귀분석 변수 선택"),
                     
                     fluidRow(
                       column(6,
                              p(strong("설명변수 (독립변수) - 수치형")),
                              p("하나 이상 선택해야 합니다."),
                              selectizeInput(
                                "reg_explanatory",
                                "변수 선택 (검색 가능):",
                                choices = numeric_vars,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "변수명을 입력하거나 선택하세요")
                              ),
                              fluidRow(
                                column(6, actionButton("reg_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                                column(6, actionButton("reg_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                              )
                       ),
                       
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
      
      classification = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "🎯 분류분석 변수 선택"),
                     
                     fluidRow(
                       column(6,
                              p(strong("설명변수 (독립변수)")),
                              p("수치형 또는 범주형, 하나 이상 선택"),
                              selectizeInput(
                                "class_explanatory",
                                "변수 선택 (검색 가능):",
                                choices = c(numeric_vars, character_vars),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "변수명을 입력하거나 선택하세요")
                              ),
                              fluidRow(
                                column(6, actionButton("class_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                                column(6, actionButton("class_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                              )
                       ),
                       
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
      
      cluster = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "🎲 군집분석할 변수 선택 (수치형)"),
                     p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
                     selectizeInput(
                       "cluster_variables",
                       "변수 선택 (검색 가능):",
                       choices = numeric_vars,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "변수명을 입력하거나 선택하세요")
                     ),
                     fluidRow(
                       column(6, actionButton("cluster_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                       column(6, actionButton("cluster_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                     )
                 )
          )
        )
      },
      
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
                                choices = unique(c(date_vars, names(data_store$original_data))),
                                selected = NULL
                              )
                       ),
                       
                       column(6,
                              p(strong("값 변수 (수치형)")),
                              p("분석할 수치형 변수, 하나 이상 선택"),
                              selectizeInput(
                                "ts_value",
                                "변수 선택 (검색 가능):",
                                choices = numeric_vars,
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "변수명을 입력하거나 선택하세요")
                              ),
                              fluidRow(
                                column(6, actionButton("ts_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                                column(6, actionButton("ts_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                              )
                       )
                     )
                 )
          )
        )
      },
      
      dimension = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "🔍 차원 축소 변수 선택 (수치형)"),
                     p("분석에 포함할 수치형 변수를 선택하세요. 최소 2개 이상 선택해야 합니다."),
                     selectizeInput(
                       "dim_variables",
                       "변수 선택 (검색 가능):",
                       choices = numeric_vars,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "변수명을 입력하거나 선택하세요")
                     ),
                     fluidRow(
                       column(6, actionButton("dim_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                       column(6, actionButton("dim_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                     )
                 )
          )
        )
      },
      
      survival = {
        fluidRow(
          column(12,
                 div(class = "variable-selector-box",
                     div(class = "variable-selector-title", "⏱️ 생존 분석 변수 선택"),
                     
                     fluidRow(
                       column(3,
                              p(strong("시간변수")),
                              p("follow-up time (수치형)"),
                              radioButtons(
                                "surv_time",
                                "시간변수:",
                                choices = numeric_vars,
                                selected = NULL
                              )
                       ),
                       
                       column(3,
                              p(strong("사건변수")),
                              p("사건 여부 또는 상태값"),
                              radioButtons(
                                "surv_event",
                                "사건변수:",
                                choices = c(numeric_vars, character_vars),
                                selected = NULL
                              )
                       ),
                       
                       column(3,
                              p(strong("사건값")),
                              p("예: pbc status에서 2를 사건 발생으로 처리"),
                              textInput(
                                "event_value",
                                "사건 발생값:",
                                value = ""
                              )
                       ),
                       
                       column(3,
                              p(strong("공변량 (선택)")),
                              p("분석에 포함할 변수"),
                              selectizeInput(
                                "surv_covariate",
                                "변수 선택 (검색 가능):",
                                choices = c(numeric_vars, character_vars),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "변수명을 입력하거나 선택하세요")
                              ),
                              fluidRow(
                                column(6, actionButton("surv_select_all",   "전체 선택", class = "btn btn-sm btn-default")),
                                column(6, actionButton("surv_deselect_all", "선택 해제", class = "btn btn-sm btn-default"))
                              )
                       )
                     )
                 )
          )
        )
      },
      
      NULL
    )
    
    return(variable_ui)
  })
  
  # 상관분석
  observeEvent(input$corr_select_all,   { updateSelectizeInput(session, "corr_variables", choices = numeric_vars_current(), selected = numeric_vars_current()) })
  observeEvent(input$corr_deselect_all, { updateSelectizeInput(session, "corr_variables", selected = character(0)) })
  
  # 회귀분석
  observeEvent(input$reg_select_all,    { updateSelectizeInput(session, "reg_explanatory", choices = numeric_vars_current(), selected = numeric_vars_current()) })
  observeEvent(input$reg_deselect_all,  { updateSelectizeInput(session, "reg_explanatory", selected = character(0)) })
  
  # 분류분석
  observeEvent(input$class_select_all,  { updateSelectizeInput(session, "class_explanatory", choices = c(numeric_vars_current(), character_vars_current()), selected = c(numeric_vars_current(), character_vars_current())) })
  observeEvent(input$class_deselect_all,{ updateSelectizeInput(session, "class_explanatory", selected = character(0)) })
  
  # 군집분석
  observeEvent(input$cluster_select_all,   { updateSelectizeInput(session, "cluster_variables", choices = numeric_vars_current(), selected = numeric_vars_current()) })
  observeEvent(input$cluster_deselect_all, { updateSelectizeInput(session, "cluster_variables", selected = character(0)) })
  
  # 시계열
  observeEvent(input$ts_select_all,    { updateSelectizeInput(session, "ts_value", choices = numeric_vars_current(), selected = numeric_vars_current()) })
  observeEvent(input$ts_deselect_all,  { updateSelectizeInput(session, "ts_value", selected = character(0)) })
  
  # 생존분석
  observeEvent(input$surv_select_all,   { updateSelectizeInput(session, "surv_covariate", choices = c(numeric_vars_current(), character_vars_current()), selected = c(numeric_vars_current(), character_vars_current())) })
  observeEvent(input$surv_deselect_all, { updateSelectizeInput(session, "surv_covariate", selected = character(0)) })
  
  # ============================================================================
  # 4. 검정 유형별 설명 출력
  # ============================================================================
  
  output$test_description_output <- renderUI({
    if (input$test_type == "" || is.null(input$test_type)) {
      return(
        div(style = "color: #999; margin-top: 10px;",
            "검정 유형을 선택하면 설명이 여기 나타납니다."
        )
      )
    }
    
    test_descriptions <- list(
      mean_test = list(
        description = "두 개 이상의 그룹 간 평균의 차이가 통계적으로 유의미한지 검정합니다.",
        conditions = "• 두 그룹: t-검정 사용",
        conditions2 = "• 세 개 이상 그룹: ANOVA 사용",
        use_case = "예) 남녀 학생의 수학점수 비교 또는 A, B, C 처리 방법의 효과 비교"
      ),
      chi_square = list(
        description = "두 개의 범주형 변수 간의 독립성을 검정합니다.",
        conditions = "• 범주형 변수 2개 필요",
        conditions2 = "• 기대도수 ≥ 5 권장",
        use_case = "예) 성별과 선호도의 관계, 지역과 구매 의사의 관계"
      ),
      correlation_test = list(
        description = "두 개의 연속형 변수 간의 선형 관계가 통계적으로 유의미한지 검정합니다.",
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
  # 5. 검정 유형별 변수 선택 UI
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
    
    # 빈 경우 안내 메시지용 더미값
    if (length(numeric_vars) == 0)   numeric_vars   <- c("수치형 변수 없음" = "")
    if (length(character_vars) == 0) character_vars <- c("범주형 변수 없음" = "")
    
    switch(
      input$test_type,
      
      mean_test = {
        fluidRow(
          column(6,
                 div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                     p(strong("📌 그룹 변수 (설명변수)")),
                     p("비교하려는 그룹을 나타내는 범주형 변수"),
                     p("예) 성별, 처리방법, 지역"),
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
                     p("예) 점수, 몸무게, 혈압"),
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
      
      chi_square = {
        fluidRow(
          column(6,
                 div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                     p(strong("📌 변수 1 (범주형)")),
                     p("첫 번째 범주형 변수"),
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
      
      correlation_test = {
        fluidRow(
          column(6,
                 div(style = "background-color: #f5f5f5; padding: 15px; border-radius: 4px;",
                     p(strong("📌 변수 1 (연속형)")),
                     p("첫 번째 수치형 변수"),
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
  # 6. 데이터 미리보기
  # ============================================================================
  
  output$data_preview <- renderTable({
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    
    head(data_store$original_data, 10)
  })
  
  # ============================================================================
  # 7. 품질 진단 실행
  # ============================================================================
  
  observeEvent(input$run_diagnosis, {
    req(data_store$original_data)
    
    data_store$quality_report <- assess_data_quality(data_store$original_data)
    
    data_store$suitability_result <- evaluate_suitability(
      quality_report = data_store$quality_report,
      purpose        = input$analysis_purpose,
      data           = data_store$original_data,
      selected_vars  = list(
        # 공통
        purpose = input$analysis_purpose,
        
        # EDA / 기초통계 / 범주형
        selected_vars = input$eda_vars,
        variables     = input$eda_vars,
        eda_vars      = input$eda_vars,
        
        # 가설 검정
        test_type      = input$test_type,
        group_var      = input$mean_group,
        group_variable = input$mean_group,
        value_var      = input$mean_value,
        chi_var1       = input$chi_var1,
        chi_var2       = input$chi_var2,
        corr_var1      = input$corr_var1,
        corr_var2      = input$corr_var2,
        
        # 상관분석
        corr_variables = input$corr_variables,
        
        # 회귀 / 분류 공통
        response_var   = input$reg_response %||% input$class_response,
        y_var          = input$reg_response %||% input$class_response,
        predictor_vars = input$reg_explanatory %||% input$class_explanatory,
        x_vars         = input$reg_explanatory %||% input$class_explanatory,
        
        # 군집 / 차원축소
        cluster_vars = input$cluster_variables,
        dim_vars     = input$dim_variables,
        
        # 시계열
        time_var      = input$ts_time,
        value_var     = input$ts_value,
        analysis_vars = input$ts_value,
        
        # 생존
        survival_time_var = input$surv_time,
        event_var         = input$surv_event,
        event_value       = input$event_value,
        covariates        = input$surv_covariate
      )
    )
    
    updateTabsetPanel(session, "main_tabs", selected = "Step 2: 진단 결과")
    
    showNotification(
      "진단이 완료되었습니다!",
      type = "message",
      duration = 3
    )
  })
  
  # ============================================================================
  # 8. 진단 결과 출력
  # ============================================================================
  
  output$diagnosis_results <- renderUI({
    if (is.null(data_store$quality_report)) {
      return(
        div(class = "no-data",
            "아직 진단을 실행하지 않았습니다.",
            br(),
            "Step 1에서 파일을 업로드하고 진단을 시작하세요."
        )
      )
    }
    
    report      <- data_store$quality_report
    suitability <- data_store$suitability_result
    
    # assess_data_quality() 반환 구조에서 필요한 데이터 추출
    score      <- report$total_score
    missing_df <- report$details$missing$details$variable_missing
    missing_df <- missing_df[missing_df$missing_count > 0, ]
    outlier_df <- report$details$outlier$details$outlier_table
    outlier_df <- outlier_df[outlier_df$outlier_count > 0, ]
    type_df    <- report$details$type$details$type_issue_table
    dup_count  <- report$details$duplicate$details$duplicate_count
    major_issues <- report$major_issues
    
    # 분석 목적 한글 레이블
    purpose_labels <- c(
      basic_stats    = "기초 통계 분석",
      visualization  = "범주형 변수 빈도 분석",
      advanced_ml    = "통계적 검정 / 가설 검정",
      correlation    = "상관관계 분석",
      regression     = "회귀분석",
      classification = "분류분석",
      cluster        = "군집분석",
      time_series    = "시계열 분석",
      dimension      = "차원 축소 / 변수 구조 탐색",
      survival       = "생존 분석"
    )
    p_label <- if (!is.null(input$analysis_purpose) &&
                   input$analysis_purpose %in% names(purpose_labels))
      purpose_labels[[input$analysis_purpose]]
    else "미선택"
    
    list(
      # ------------------------------------------------------------------
      # 품질 점수
      # ------------------------------------------------------------------
      fluidRow(
        column(12, div(class = "section-title", "📈 데이터 품질 점수"))
      ),
      fluidRow(
        column(4,
               div(class = "quality-score-display", score, "점")
        ),
        column(8,
               div(
                 p("총 100점 중 획득한 점수입니다."),
                 p(strong("평가 기준: "),
                   "90~100점 (우수), 70~89점 (양호), 50~69점 (주의), 0~49점 (부적합)")
               )
        )
      ),
      
      # ------------------------------------------------------------------
      # 주요 경고
      # ------------------------------------------------------------------
      fluidRow(
        column(12, div(class = "section-title", "⚠️ 주요 경고"))
      ),
      
      # 결측치
      fluidRow(
        column(12,
               if (!is.null(missing_df) && nrow(missing_df) > 0) {
                 div(class = "warning-box",
                     div(class = "warning-box-title", "❌ 결측치 발견"),
                     lapply(seq_len(nrow(missing_df)), function(i) {
                       row <- missing_df[i, ]
                       div(class = "warning-box-content",
                           HTML(paste0(
                             "<strong>", row$variable, "</strong> 변수에 ",
                             row$missing_percent, "% (", row$missing_count,
                             "개)의 결측치가 존재합니다."
                           ))
                       )
                     })
                 )
               } else {
                 div(class = "warning-box",
                     div(class = "warning-box-content", "✅ 결측치가 없습니다."))
               }
        )
      ),
      
      # 이상치
      fluidRow(
        column(12,
               if (!is.null(outlier_df) && nrow(outlier_df) > 0) {
                 div(class = "warning-box",
                     div(class = "warning-box-title", "❌ 이상치 발견"),
                     lapply(seq_len(nrow(outlier_df)), function(i) {
                       row <- outlier_df[i, ]
                       div(class = "warning-box-content",
                           HTML(paste0(
                             "<strong>", row$variable, "</strong> 변수에서 ",
                             row$outlier_percent, "% (", row$outlier_count,
                             "개)의 이상치가 발견되었습니다."
                           ))
                       )
                     })
                 )
               } else {
                 div(class = "warning-box",
                     div(class = "warning-box-content", "✅ 이상치가 없습니다."))
               }
        )
      ),
      
      # 자료형 오류
      fluidRow(
        column(12,
               if (!is.null(type_df) && nrow(type_df) > 0) {
                 div(class = "warning-box",
                     div(class = "warning-box-title", "❌ 자료형 오류 의심 변수"),
                     lapply(seq_len(nrow(type_df)), function(i) {
                       row <- type_df[i, ]
                       div(class = "warning-box-content",
                           HTML(paste0(
                             "<strong>", row$variable, "</strong> 변수: ", row$issue
                           ))
                       )
                     })
                 )
               } else {
                 div(class = "warning-box",
                     div(class = "warning-box-content", "✅ 자료형 오류가 없습니다."))
               }
        )
      ),
      
      # 중복 행
      if (!is.null(dup_count) && dup_count > 0) {
        fluidRow(
          column(12,
                 div(class = "warning-box",
                     div(class = "warning-box-title", "❌ 중복 행 발견"),
                     div(class = "warning-box-content",
                         HTML(paste0("총 <strong>", dup_count,
                                     "건</strong>의 중복 행이 존재합니다. ",
                                     "분석 목적에 따라 중복 제거 여부를 결정하세요."))
                     )
                 )
          )
        )
      },
      
      # ------------------------------------------------------------------
      # 주요 이슈 요약
      # ------------------------------------------------------------------
      if (!is.null(major_issues) && nrow(major_issues) > 0 &&
          !(nrow(major_issues) == 1 && major_issues$문제항목[1] == "없음")) {
        fluidRow(
          column(12,
                 div(class = "section-title", "📋 주요 이슈 요약"),
                 lapply(seq_len(nrow(major_issues)), function(i) {
                   r <- major_issues[i, ]
                   box_class <- if (r$상태 == "위험") "warning-box" else "warning-box"
                   icon <- if (r$상태 == "위험") "⚠️" else "🔔"
                   div(class = box_class,
                       div(class = "warning-box-title",
                           HTML(paste0(icon, " [", r$상태, "] ", r$문제항목))),
                       div(class = "warning-box-content", r$설명)
                   )
                 })
          )
        )
      },
      
      # ------------------------------------------------------------------
      # 적합성 평가
      # ------------------------------------------------------------------
      fluidRow(
        column(12, div(class = "section-title", "📋 분석 적합성 평가"))
      ),
      
      # 적합성 점수
      fluidRow(
        column(4,
               div(class = "quality-score-display",
                   style = switch(suitability$color,
                                  success   = "color:#1D9E75;",
                                  warning   = "color:#f5821d;",
                                  danger    = "color:#E24B4A;",
                                  "color:#aaaaaa;"
                   ),
                   suitability$status
               )
        ),
        column(8,
               div(
                 p(strong("평가 기준: "),
                   "적합 / 부분 적합 / 부적합"),
                 p(strong("분석 목적: "), p_label)
               )
        )
      ),
      
      # 평가 메시지
      fluidRow(
        column(12,
               div(class = paste("suitability-box", suitability$color),
                   div(class = "suitability-status",
                       HTML(paste0("📌 ", suitability$message)))
               )
        )
      ),
      
      # 권장사항 항목별 박스
      if (length(suitability$recommendations) > 0) {
        fluidRow(
          column(12,
                 div(class = "warning-box-title", "💡 권장사항"),
                 lapply(suitability$recommendations, function(rec) {
                   div(class = "warning-box",
                       div(class = "warning-box-content", rec))
                 })
          )
        )
      },
      
      # ------------------------------------------------------------------
      # 데이터셋 정보
      # ------------------------------------------------------------------
      fluidRow(
        column(12,
               div(class = "section-title", "📊 데이터셋 정보"),
               tableOutput("dataset_info")
        )
      )
    )
  })
  
  # ============================================================================
  # 9. 데이터셋 정보 표
  # ============================================================================
  
  output$dataset_info <- renderTable({
    if (is.null(data_store$original_data)) {
      return(NULL)
    }
    
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
  # 10. HTML 보고서 생성 및 다운로드
  # ============================================================================
  
  output$download_report <- downloadHandler(
    filename = function() {
      base <- if (!is.null(input$file_upload$name)) {
        tools::file_path_sans_ext(input$file_upload$name)
      } else {
        "데이터"
      }
      purpose_labels <- c(
        regression = "회귀분석", classification = "분류분석",
        time_series = "시계열분석", survival = "생존분석",
        correlation = "상관분석", cluster = "군집분석",
        basic_stats = "기초통계", visualization = "빈도분석",
        advanced_ml = "가설검정", dimension = "차원축소"
      )
      p_label <- purpose_labels[input$analysis_purpose] %||% "분석"
      paste0(base, "_", p_label, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".html")
    },
    content = function(file) {
      req(data_store$quality_report)
      req(data_store$suitability_result)
      req(data_store$original_data)
      
      tryCatch({
        generate_html_report(
          file          = file,
          data          = data_store$original_data,
          report        = data_store$quality_report,
          suitability   = data_store$suitability_result,
          purpose       = input$analysis_purpose,
          filename      = input$file_upload$name,
          selected_vars = data_store$suitability_result$selected_vars
        )
      }, error = function(e) {
        showNotification(
          paste0("보고서 생성 중 오류 발생: ", e$message),
          type = "error", duration = 7
        )
      })
    }
  )
  observeEvent(input$save_to_reports, {
    req(data_store$quality_report)
    req(data_store$suitability_result)
    req(data_store$original_data)
    
    base <- if (!is.null(input$file_upload$name)) {
      tools::file_path_sans_ext(input$file_upload$name)
    } else { "데이터" }
    
    purpose_label <- c(
      regression = "회귀분석", classification = "분류분석",
      time_series = "시계열분석", survival = "생존분석",
      correlation = "상관분석", cluster = "군집분석",
      basic_stats = "기초통계", visualization = "빈도분석",
      advanced_ml = "가설검정", dimension = "차원축소"
    )
    p_label <- purpose_label[input$analysis_purpose] %||% "분석"
    
    # 파일명 중복 방지: 시간 붙이기
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    fname <- paste0(base, "_", p_label, "_", timestamp, ".html")
    fpath <- file.path("reports", fname)
    
    tryCatch({
      generate_html_report(
        file          = fpath,
        data          = data_store$original_data,
        report        = data_store$quality_report,
        suitability   = data_store$suitability_result,
        purpose       = input$analysis_purpose,
        filename      = input$file_upload$name,
        selected_vars = data_store$suitability_result$selected_vars
      )
      data_store$report_result <- fpath
      showNotification(
        paste0("HTML 보고서가 reports/ 폴더에 저장되었습니다: ", fname),
        type = "message", duration = 5
      )
    }, error = function(e) {
      showNotification(
        paste0("보고서 저장 중 오류 발생: ", e$message),
        type = "error", duration = 7
      )
    })
  })
}