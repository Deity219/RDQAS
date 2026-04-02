library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# Source core functions when running as a standalone app (outside the package)
# ─────────────────────────────────────────────────────────────────────────────
if (!requireNamespace("dataQI", quietly = TRUE)) {
  pkg_r <- file.path(dirname(dirname(getwd())), "R")
  for (f in list.files(pkg_r, pattern = "\\.R$", full.names = TRUE)) {
    source(f)
  }
}

ui <- dashboardPage(
  skin = "blue",

  # ── Header ──────────────────────────────────────────────────────────────────
  dashboardHeader(
    title = "dataQI \u2014 \ub370\uc774\ud130 \ud488\uc9c8 \uac80\uc0ac",
    titleWidth = 320
  ),

  # ── Sidebar ─────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "sidebar",
      menuItem("\ud30c\uc77c \uc5c5\ub85c\ub4dc",    tabName = "upload",       icon = icon("upload")),
      menuItem("\ub370\uc774\ud130 \ubbf8\ub9ac\ubcf4\uae30", tabName = "preview",    icon = icon("table")),
      menuItem("\ud488\uc9c8 \ub300\uc2dc\ubcf4\ub4dc", tabName = "dashboard",  icon = icon("tachometer-alt")),
      menuItem("\uc644\uc804\uc131 \uac80\uc0ac",    tabName = "completeness", icon = icon("check-circle")),
      menuItem("\uace0\uc720\uc131 \uac80\uc0ac",    tabName = "uniqueness",   icon = icon("fingerprint")),
      menuItem("\uc720\ud6a8\uc131 \uac80\uc0ac",    tabName = "validity",     icon = icon("filter")),
      menuItem("\uc774\uc0c1\uce58 \uac80\uc0ac",    tabName = "outliers",     icon = icon("exclamation-triangle")),
      menuItem("\ubcf4\uace0\uc11c \uc800\uc7a5",    tabName = "report",       icon = icon("file-alt"))
    )
  ),

  # ── Body ─────────────────────────────────────────────────────────────────────
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .score-box   { font-size: 2em; font-weight: bold; text-align: center; padding: 10px; }
        .grade-A     { color: #27ae60; }
        .grade-B     { color: #2980b9; }
        .grade-C     { color: #f39c12; }
        .grade-D     { color: #e67e22; }
        .grade-F     { color: #c0392b; }
        .info-text   { color: #7f8c8d; font-size: 0.85em; }
      "))
    ),

    tabItems(
      # ── Upload ──────────────────────────────────────────────────────────────
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "\ud30c\uc77c \uc5c5\ub85c\ub4dc", width = 12, status = "primary", solidHeader = TRUE,
            fileInput("file", "\ub370\uc774\ud130 \ud30c\uc77c \uc120\ud0dd (CSV / Excel)",
                      accept = c(".csv", ".xlsx", ".xls"),
                      buttonLabel = "\ub2e4\uc74c",
                      placeholder = "\ud30c\uc77c\uc744 \uc120\ud0dd\ud558\uc138\uc694..."),
            checkboxInput("header", "\uccab \ud589\uc744 \ud5e4\ub354\ub85c \uc0ac\uc6a9", value = TRUE),
            conditionalPanel(
              condition = "output.is_csv",
              selectInput("sep", "\uad6c\ubd84\uc790",
                          choices = c("\ucf64\ub9c8(,)" = ",", "\uc138\ubbf8\ucf5c\ub860(;)" = ";",
                                      "\ud0ed(\\t)" = "\t", "\uc2a4\ud398\uc774\uc2a4" = " "),
                          selected = ","),
              selectInput("quote", "\ubb38\uc790\uc5f4 \uc778\uc6a9 \ubd80\ud638",
                          choices = c("\uc5c6\uc74c" = "", "\ud070\ub530\uc634\ud45c" = "\"",
                                      "\uc791\uc740\ub530\uc634\ud45c" = "'"),
                          selected = "\"")
            ),
            actionButton("load_btn", "\ub370\uc774\ud130 \ubd88\ub7ec\uc624\uae30", icon = icon("folder-open"),
                         class = "btn-primary")
          )
        ),
        fluidRow(
          uiOutput("upload_status_box")
        )
      ),

      # ── Preview ─────────────────────────────────────────────────────────────
      tabItem(
        tabName = "preview",
        fluidRow(
          box(
            title = "\ub370\uc774\ud130 \ubbf8\ub9ac\ubcf4\uae30", width = 12, status = "info", solidHeader = TRUE,
            DTOutput("data_table")
          )
        )
      ),

      # ── Dashboard ───────────────────────────────────────────────────────────
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("vbox_overall",      width = 3),
          valueBoxOutput("vbox_completeness", width = 3),
          valueBoxOutput("vbox_uniqueness",   width = 3),
          valueBoxOutput("vbox_validity",     width = 3)
        ),
        fluidRow(
          valueBoxOutput("vbox_outliers", width = 3)
        ),
        fluidRow(
          box(
            title = "\ub370\uc774\ud130 \ud488\uc9c8 \uc810\uc218 \uc694\uc57d", width = 12, status = "primary", solidHeader = TRUE,
            plotOutput("summary_plot", height = "300px")
          )
        )
      ),

      # ── Completeness ────────────────────────────────────────────────────────
      tabItem(
        tabName = "completeness",
        fluidRow(
          box(
            title = "\uc644\uc804\uc131 \uac80\uc0ac \uc2e4\uc801", width = 12, status = "success", solidHeader = TRUE,
            DTOutput("completeness_table")
          )
        ),
        fluidRow(
          box(
            title = "\uce7c\ub7fc\ubcc4 \uacb0\uce21\uce58 \ube44\uc728", width = 12, solidHeader = TRUE,
            plotOutput("completeness_plot", height = "300px")
          )
        )
      ),

      # ── Uniqueness ──────────────────────────────────────────────────────────
      tabItem(
        tabName = "uniqueness",
        fluidRow(
          box(
            title = "\uace0\uc720\uc131 \uac80\uc0ac \uc2e4\uc801", width = 12, status = "warning", solidHeader = TRUE,
            DTOutput("uniqueness_table")
          )
        ),
        fluidRow(
          box(
            title = "\uc911\ubcf5 \ud589 \ubaa9\ub85d", width = 12, solidHeader = TRUE,
            DTOutput("duplicate_table")
          )
        )
      ),

      # ── Validity ────────────────────────────────────────────────────────────
      tabItem(
        tabName = "validity",
        fluidRow(
          box(
            title = "\uc720\ud6a8\uc131 \uac80\uc0ac \uc2e4\uc801", width = 12, status = "info", solidHeader = TRUE,
            DTOutput("validity_table")
          )
        )
      ),

      # ── Outliers ────────────────────────────────────────────────────────────
      tabItem(
        tabName = "outliers",
        fluidRow(
          box(
            title = "\uc774\uc0c1\uce58 \uac80\uc0ac \uc2e4\uc801", width = 12, status = "danger", solidHeader = TRUE,
            DTOutput("outliers_table")
          )
        ),
        fluidRow(
          box(
            title = "\uc218\uce58\ud615 \uce7c\ub7fc \ubc15\uc2a4\ud50c\ub86f", width = 12, solidHeader = TRUE,
            uiOutput("boxplots_ui")
          )
        )
      ),

      # ── Report ──────────────────────────────────────────────────────────────
      tabItem(
        tabName = "report",
        fluidRow(
          box(
            title = "\uc885\ud569 \ubcf4\uace0\uc11c", width = 12, status = "primary", solidHeader = TRUE,
            verbatimTextOutput("report_text"),
            downloadButton("download_report", "\ub9ac\ud3ec\ud2b8 \ub2e4\uc6b4\ub85c\ub4dc (CSV)",
                           class = "btn-success")
          )
        )
      )
    )
  )
)
