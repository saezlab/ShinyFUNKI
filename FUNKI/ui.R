# UI
source("sub/global.R")
ui = function(request) {
  fluidPage(
    
    useShinyjs(),
    tags$head(includeScript("google-analytics.js")),
    navbarPage(
      id = "menu", title="FUNKI",collapsible=T,
      footer = column(12, align="center", "FUNKI-App 2021 (v1.0.0-beta)"),
      source("sub/01_ui_welcome.R")$value,
      source("sub/02_ui_upload.R")$value,
      source("sub/03_ui_dorothea.R")$value,
      source("sub/04_ui_progeny.R")$value,
      source("sub/07_ui_carnival.R")$value,
      source("sub/05_ui_kinact.R")$value
      ,source("sub/ui_help.R")$value
      ,source("sub/ui_contact.R")$value
      ,hr()
      ) # close navbarPage
    ) # close fluidPage
}