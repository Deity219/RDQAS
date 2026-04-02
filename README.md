# dataQI — 사용자 친화형 데이터 품질 검사 시스템

> **User-Friendly Data Quality Inspection System**

dataQI는 R 기반의 대화형 데이터 품질 검사 패키지입니다. 완전성(Completeness), 고유성(Uniqueness), 유효성(Validity), 이상치(Outlier), 일관성(Consistency) 등 5가지 핵심 품질 차원을 자동으로 평가하고, Shiny 기반의 대시보드를 통해 결과를 시각화합니다.

---

## 주요 기능

| 기능 | 설명 |
|------|------|
| **완전성 검사** | 열별 결측치(NA) 수 및 비율 분석 |
| **고유성 검사** | 중복 행 탐지 및 열별 고유값 비율 계산 |
| **유효성 검사** | 데이터 타입, 범위, 허용값, 정규식 패턴 검증 |
| **이상치 검사** | IQR 방식을 이용한 수치형 열 이상치 탐지 |
| **일관성 검사** | 사용자 정의 교차 열 규칙 검증 |
| **종합 보고서** | 모든 차원을 통합한 품질 점수 및 등급(A–F) 보고 |
| **Shiny 대시보드** | 파일 업로드부터 시각화까지 사용자 친화형 UI |

---

## 설치

```r
# devtools 또는 pak으로 설치
# install.packages("devtools")
devtools::install_github("Deity219/dataQI")
```

필요 패키지:

```r
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2",
                   "dplyr", "readr", "readxl", "scales", "shinyjs"))
```

---

## 빠른 시작

### 대화형 Shiny 앱 실행

```r
library(dataQI)
run_app()
```

### R 스크립트에서 직접 사용

```r
library(dataQI)

# 예시 데이터
df <- data.frame(
  age    = c(25, NA, 200, 30, 22),
  name   = c("Alice", "Bob", "Bob", "Dana", "Eve"),
  status = c("active", "inactive", "unknown", "active", "active")
)

# 1. 완전성 검사
comp <- check_completeness(df)
comp$completeness_score   # 0~1 점수

# 2. 고유성 검사
uniq <- check_uniqueness(df)
uniq$duplicate_count      # 중복 행 수

# 3. 유효성 검사 (규칙 지정)
rules <- list(
  age    = list(type = "numeric", min = 0, max = 150),
  status = list(allowed = c("active", "inactive"))
)
valid <- check_validity(df, rules)
valid$validity_score

# 4. 이상치 검사
outl <- check_outliers(df)
outl$outlier_score

# 5. 일관성 검사
cons_rules <- list(
  valid_age = list(expr = "age >= 18", description = "성인 여부 확인")
)
cons <- check_consistency(df, cons_rules)
cons$consistency_score

# 6. 종합 보고서
report <- generate_report(df,
                           validity_rules    = rules,
                           consistency_rules = cons_rules)
print(report)
#> === dataQI Data Quality Report ===
#> Overall Quality Score : 76.2%  [C]
#> ...
```

---

## 품질 등급 기준

| 등급 | 점수 범위 |
|------|-----------|
| **A** | 95% 이상 |
| **B** | 85% – 94% |
| **C** | 70% – 84% |
| **D** | 50% – 69% |
| **F** | 50% 미만 |

---

## 프로젝트 구조

```
dataQI/
├── R/
│   ├── check_completeness.R   # 완전성 검사
│   ├── check_uniqueness.R     # 고유성 검사
│   ├── check_validity.R       # 유효성 검사
│   ├── check_outliers.R       # 이상치 검사
│   ├── check_consistency.R    # 일관성 검사
│   ├── generate_report.R      # 종합 보고서 생성
│   └── run_app.R              # Shiny 앱 실행
├── inst/app/
│   ├── ui.R                   # Shiny UI
│   └── server.R               # Shiny 서버 로직
├── tests/testthat/            # 단위 테스트
├── DESCRIPTION
├── NAMESPACE
└── README.md
```

---

## 테스트 실행

```r
devtools::test()
```

---

## 라이선스

MIT

