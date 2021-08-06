# UI
source("sub/global_analysis.R")
source("sub/global_plots.R")
source("sub/global_sup.R")
source("sub/global_modules.R")
ui = function(request) {
  fluidPage(
    
    useShinyjs(),
    tags$head(includeScript("google-analytics.js")),
    navbarPage(
      id = "menu",
      windowTitle = "FUNKI",
      title = div(img(src="logo_funki.png",width="60", height="30"), "FUNKI"), 
      collapsible=T,
      footer = column(12, align="center", 
                      div(img(src="logo_funki.png",width="60", height="30"), 
                                              "FUNKI-App 2021 (v1.0.0-beta)",
                          img(src="logo_saezlab.png",width="30", height="30"))),
      source("sub/01_ui_welcome.R")$value,
      source("sub/02_ui_upload.R")$value,
      source("sub/03_ui_dorothea.R")$value,
      source("sub/04_ui_progeny.R")$value,
      source("sub/05_ui_kinact.R")$value,
      source("sub/06_ui_carnival.R")$value,
      source("sub/07_ui_cosmos.R")$value
      ,source("sub/ui_help.R")$value
      ,source("sub/ui_contact.R")$value
      ,hr()
      ) # close navbarPage
    ) # close fluidPage
}