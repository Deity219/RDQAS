# app.R
# RDQAS - R Data Quality Assessment System
# 실행: RStudio에서 RDQAS.Rproj를 연 뒤 app.R을 Run App으로 실행

# ============================================================
# 1. 실행 위치 확인
# ============================================================
if (!dir.exists("R")) {
  stop(
    "R/ 폴더를 찾을 수 없습니다.\n",
    "현재 작업 디렉토리: ", getwd(), "\n\n",
    "RStudio에서 RDQAS.Rproj를 연 뒤 app.R을 Run App으로 실행하거나,\n",
    "RDQAS 프로젝트 루트에서 source(\"app.R\")를 실행해주세요.",
    call. = FALSE
  )
}

required_dirs <- c("R/quality", "R/suitability")
missing_dirs <- required_dirs[!dir.exists(required_dirs)]

if (length(missing_dirs) > 0) {
  stop(
    "필수 폴더를 찾을 수 없습니다: ",
    paste(missing_dirs, collapse = ", "),
    "\n현재 작업 디렉토리: ", getwd(),
    call. = FALSE
  )
}

# ============================================================
# 2. 소스 파일 로드
#    순서 중요: packages → quality → suitability → report → ui → server
# ============================================================
safe_source <- function(path) {
  tryCatch(
    source(path, local = FALSE),
    error = function(e) {
      message("\n[source 오류] 파일: ", path)
      message("[원인] ", conditionMessage(e))
      stop("source 실패: ", path, call. = FALSE)
    }
  )
}

# 패키지 로드 및 패키지 누락 안내는 R/01_packages.R에서 처리
safe_source("R/01_packages.R")

# Quality 함수들
quality_files <- sort(list.files(
  path = "R/quality",
  pattern = "\\.R$",
  full.names = TRUE
))

if (length(quality_files) == 0) {
  stop("R/quality 폴더에 R 파일이 없습니다.", call. = FALSE)
}

invisible(lapply(quality_files, safe_source))

# Suitability 함수들
suitability_files <- sort(list.files(
  path = "R/suitability",
  pattern = "\\.R$",
  full.names = TRUE
))

if (length(suitability_files) == 0) {
  stop("R/suitability 폴더에 R 파일이 없습니다.", call. = FALSE)
}

invisible(lapply(suitability_files, safe_source))

# Report, UI, Server
safe_source("R/02_report.R")
safe_source("R/03_ui.R")
safe_source("R/04_server.R")

# ============================================================
# 3. reports/ 폴더 준비
# ============================================================
if (!dir.exists("reports")) {
  dir.create("reports", recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# 4. 앱 실행
# ============================================================
shiny::shinyApp(ui = ui, server = server)