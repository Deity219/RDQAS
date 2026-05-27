# R/01_packages.R
# RDQAS에서 사용하는 패키지 확인 및 로드

# ============================================================
# 1. 필수 패키지 목록
# ============================================================
required_packages <- c(
  "shiny",
  "shinyBS",
  "ggplot2",
  "dplyr",
  "tidyr",
  "knitr",
  "rmarkdown",
  "readxl",
  "readr",
  "DT",
  "shinydashboard",
  "shinyjs",
  "plotly",
  "htmltools",
  "base64enc",
  "scales",
  "lubridate",
  "survival"
)

# ============================================================
# 2. 설치 여부 확인
# ============================================================
missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]

if (length(missing_packages) > 0) {
  message("\n============================================")
  message("다음 패키지가 설치되어 있지 않습니다:")
  message("  ", paste(missing_packages, collapse = ", "))
  message("\n아래 명령어로 먼저 설치한 뒤 다시 실행해주세요:")
  message(
    '  install.packages(c("',
    paste(missing_packages, collapse = '", "'),
    '"))'
  )
  message("============================================\n")
  
  stop("필수 패키지 누락", call. = FALSE)
}

# ============================================================
# 3. 패키지 로드
# ============================================================
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}))

message("필수 패키지 로드 완료")