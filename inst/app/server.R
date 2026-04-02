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
} else {
  library(dataQI)
}

server <- function(input, output, session) {

  # ── Reactive state ──────────────────────────────────────────────────────────
  rv <- reactiveValues(data = NULL, report = NULL, filename = "")

  # Detect CSV vs Excel for conditional UI
  output$is_csv <- reactive({
    req(input$file)
    tolower(tools::file_ext(input$file$name)) == "csv"
  })
  outputOptions(output, "is_csv", suspendWhenHidden = FALSE)

  # ── Load data ───────────────────────────────────────────────────────────────
  observeEvent(input$load_btn, {
    req(input$file)
    path <- input$file$datapath
    ext  <- tolower(tools::file_ext(input$file$name))

    tryCatch({
      df <- if (ext == "csv") {
        utils::read.csv(path,
                        header = input$header,
                        sep    = input$sep,
                        quote  = input$quote,
                        stringsAsFactors = FALSE,
                        check.names = FALSE)
      } else {
        readxl::read_excel(path, col_names = input$header)
      }
      rv$data     <- as.data.frame(df)
      rv$filename <- input$file$name
      rv$report   <- generate_report(rv$data)
      showNotification("\ub370\uc774\ud130\ub97c \uc131\uacf5\uc801\uc73c\ub85c \ubd88\ub7ec\uc625\ub2c8\ub2e4!", type = "message")
      updateTabItems(session, "sidebar", "dashboard")
    }, error = function(e) {
      showNotification(paste("\uc624\ub958:", conditionMessage(e)), type = "error")
    })
  })

  # Helper: require data to be loaded
  req_data <- function() req(rv$data, rv$report)

  # ── Upload status ───────────────────────────────────────────────────────────
  output$upload_status_box <- renderUI({
    if (is.null(rv$data)) return(NULL)
    box(
      title = "\ub85c\ub4dc \uc644\ub8cc", width = 12, status = "success", solidHeader = TRUE,
      tags$p(tags$b("\ud30c\uc77c:"),   rv$filename),
      tags$p(tags$b("\ud589 \uc218:"),  format(nrow(rv$data),  big.mark = ",")),
      tags$p(tags$b("\uc5f4 \uc218:"),  format(ncol(rv$data),  big.mark = ",")),
      tags$p(tags$b("\uc5f4 \uc774\ub984:"),
             paste(names(rv$data), collapse = ", "))
    )
  })

  # ── Preview ─────────────────────────────────────────────────────────────────
  output$data_table <- renderDT({
    req_data()
    datatable(rv$data, options = list(scrollX = TRUE, pageLength = 15),
              rownames = FALSE, filter = "top")
  })

  # ── Value boxes ─────────────────────────────────────────────────────────────
  make_vbox <- function(label, score, icon_name, color) {
    if (is.null(score) || is.na(score)) {
      return(valueBox("N/A", label, icon = icon(icon_name), color = "gray"))
    }
    pct   <- sprintf("%.1f%%", score * 100)
    grade <- dataQI:::.score_to_grade(score)
    col   <- switch(grade, A = "green", B = "blue", C = "yellow", D = "orange", "red")
    valueBox(paste0(pct, " (", grade, ")"), label, icon = icon(icon_name), color = col)
  }

  output$vbox_overall <- renderValueBox({
    req_data()
    make_vbox("\uc885\ud569 \ud488\uc9c8 \uc810\uc218", rv$report$overall_score, "star", "purple")
  })
  output$vbox_completeness <- renderValueBox({
    req_data()
    make_vbox("\uc644\uc804\uc131", rv$report$completeness$completeness_score, "check-circle", "green")
  })
  output$vbox_uniqueness <- renderValueBox({
    req_data()
    make_vbox("\uace0\uc720\uc131", rv$report$uniqueness$uniqueness_score, "fingerprint", "blue")
  })
  output$vbox_validity <- renderValueBox({
    req_data()
    make_vbox("\uc720\ud6a8\uc131", rv$report$validity$validity_score, "filter", "yellow")
  })
  output$vbox_outliers <- renderValueBox({
    req_data()
    make_vbox("\uc774\uc0c1\uce58 \ud655\uc778", rv$report$outliers$outlier_score, "exclamation-triangle", "red")
  })

  # ── Summary plot ─────────────────────────────────────────────────────────────
  output$summary_plot <- renderPlot({
    req_data()
    df <- rv$report$summary
    df$score_pct <- df$score * 100
    df$dimension_kr <- c(
      completeness = "\uc644\uc804\uc131",
      uniqueness   = "\uace0\uc720\uc131",
      validity     = "\uc720\ud6a8\uc131",
      outliers     = "\uc774\uc0c1\uce58 \ud655\uc778",
      consistency  = "\uc77c\uad00\uc131"
    )[df$dimension]
    df$dimension_kr[is.na(df$dimension_kr)] <- df$dimension[is.na(df$dimension_kr)]

    fill_colors <- ifelse(df$score >= 0.95, "#27ae60",
                   ifelse(df$score >= 0.85, "#2980b9",
                   ifelse(df$score >= 0.70, "#f39c12",
                   ifelse(df$score >= 0.50, "#e67e22", "#c0392b"))))

    ggplot(df, aes(x = reorder(dimension_kr, score_pct), y = score_pct, fill = dimension_kr)) +
      geom_col(show.legend = FALSE, fill = fill_colors) +
      geom_text(aes(label = sprintf("%.1f%%", score_pct)), hjust = -0.1, size = 4.5) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 115), labels = function(x) paste0(x, "%")) +
      labs(x = NULL, y = "\uc810\uc218 (%)", title = "\ub370\uc774\ud130 \ud488\uc9c8 \ucc28\uc6d0\ubcc4 \uc810\uc218") +
      theme_minimal(base_size = 13)
  })

  # ── Completeness ─────────────────────────────────────────────────────────────
  output$completeness_table <- renderDT({
    req_data()
    df <- rv$report$completeness$column_stats
    df$missing_pct <- paste0(df$missing_pct, "%")
    df$present_pct <- paste0(df$present_pct, "%")
    datatable(df, rownames = FALSE,
              colnames = c("\ucceec\ub7fc", "\uacb0\uce21 \uc218", "\uacb0\uce21 \ube44\uc728",
                           "\uc874\uc7ac \uc218", "\uc874\uc7ac \ube44\uc728"))
  })

  output$completeness_plot <- renderPlot({
    req_data()
    df <- rv$report$completeness$column_stats
    ggplot(df, aes(x = reorder(column, missing_pct), y = missing_pct, fill = missing_pct > 0)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("FALSE" = "#27ae60", "TRUE" = "#e74c3c")) +
      coord_flip() +
      labs(x = NULL, y = "\uacb0\uce21\uce58 \ube44\uc728 (%)", title = "\ucceec\ub7fc\ubcc4 \uacb0\uce21\uce58 \ube44\uc728") +
      theme_minimal(base_size = 12)
  })

  # ── Uniqueness ───────────────────────────────────────────────────────────────
  output$uniqueness_table <- renderDT({
    req_data()
    df <- rv$report$uniqueness$column_stats
    df$unique_ratio <- paste0(round(df$unique_ratio * 100, 1), "%")
    datatable(df, rownames = FALSE,
              colnames = c("\ucceec\ub7fc", "\uace0\uc720\uac12 \uc218", "\uace0\uc720 \ube44\uc728"))
  })

  output$duplicate_table <- renderDT({
    req_data()
    dups <- rv$report$uniqueness$duplicate_rows
    if (nrow(dups) == 0) {
      datatable(data.frame("\ub300\uc0c1\uc5c6\uc74c" = "\uc911\ubcf5 \ud589\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.",
                           check.names = FALSE),
                rownames = FALSE)
    } else {
      datatable(dups, rownames = TRUE, options = list(scrollX = TRUE))
    }
  })

  # ── Validity ─────────────────────────────────────────────────────────────────
  output$validity_table <- renderDT({
    req_data()
    df <- rv$report$validity$column_stats
    df$validity_ratio <- paste0(round(df$validity_ratio * 100, 1), "%")
    datatable(df, rownames = FALSE,
              colnames = c("\ucceec\ub7fc", "\ub370\uc774\ud130 \ud0c0\uc785", "\uc720\ud6a8\ud558\uc9c0 \uc54a\uc740 \uc218", "\uc720\ud6a8\uc131 \ube44\uc728"))
  })

  # ── Outliers ─────────────────────────────────────────────────────────────────
  output$outliers_table <- renderDT({
    req_data()
    df <- rv$report$outliers$column_stats
    if (nrow(df) == 0) {
      return(datatable(data.frame("\ub300\uc0c1\uc5c6\uc74c" = "\uc218\uce58\ud615 \ucceec\ub7fc\uc774 \uc5c6\uc2b5\ub2c8\ub2e4.",
                                  check.names = FALSE), rownames = FALSE))
    }
    df$lower_bound   <- round(df$lower_bound, 4)
    df$upper_bound   <- round(df$upper_bound, 4)
    df$outlier_ratio <- paste0(round(df$outlier_ratio * 100, 1), "%")
    datatable(df, rownames = FALSE,
              colnames = c("\ucceec\ub7fc", "\ud558\ud55c \uacbd\uacc4", "\uc0c1\ud55c \uacbd\uacc4",
                           "\uc774\uc0c1\uce58 \uc218", "\uc774\uc0c1\uce58 \ube44\uc728"))
  })

  output$boxplots_ui <- renderUI({
    req_data()
    numeric_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    if (length(numeric_cols) == 0) {
      return(tags$p("\uc218\uce58\ud615 \ucceec\ub7fc\uc774 \uc5c6\uc2b5\ub2c8\ub2e4."))
    }
    plot_output_list <- lapply(numeric_cols, function(col) {
      plotOutput(paste0("boxplot_", col), height = "200px")
    })
    do.call(fluidRow, lapply(numeric_cols, function(col) {
      column(4, plotOutput(paste0("boxplot_", col), height = "220px"))
    }))
  })

  observe({
    req_data()
    numeric_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
    for (col in numeric_cols) {
      local({
        .col <- col
        output[[paste0("boxplot_", .col)]] <- renderPlot({
          df_plot <- data.frame(x = rv$data[[.col]])
          ggplot(df_plot, aes(x = "", y = x)) +
            geom_boxplot(fill = "#3498db", outlier.colour = "#e74c3c",
                         outlier.shape = 16, outlier.size = 2) +
            labs(x = NULL, y = .col, title = .col) +
            theme_minimal(base_size = 11)
        })
      })
    }
  })

  # ── Report ───────────────────────────────────────────────────────────────────
  output$report_text <- renderPrint({
    req_data()
    print(rv$report)
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("dataQI_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req_data()
      utils::write.csv(rv$report$summary, file, row.names = FALSE)
    }
  )
}
