# app.R

source("R/01_packages.R")

quality_files <- list.files(
  path = "R/quality",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(sort(quality_files), source))

suitability_files <- list.files(
  path = "R/suitability",
  pattern = "\\.R$",
  full.names = TRUE
)
invisible(lapply(sort(suitability_files), source))

source("R/02_report.R")
source("R/03_ui.R")
source("R/04_server.R")

shinyApp(ui = ui, server = server)