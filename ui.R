
# UI
source("sub/global.R")

ui = fluidPage(
  useShinyjs(),
  #shinyjs::addClass(id = "welcome", class = "navbar-right"),
  #theme = shinythemes::shinytheme("spacelab"),
  navbarPage(
    id = "menu", title="FUNKI",collapsible=T,
    footer = column(12, align="center", "FUNKI-App 2018 (version: 0.1)"),
    source("sub/01_ui_welcome.R")$value,
    source("sub/02_ui_upload.R")$value,
    source("sub/03_ui_dorothea.R")$value
    #,source("sub/04_ui_progeny.R")$value
    #,source("sub/05_ui_kinact.R")$value
    #,source("sub/06_ui_integration.R")$value
    ,source("sub/ui_help.R")$value
    ,source("sub/ui_contact.R")$value
    ,hr()
    ) # close navbarPage
  ) # close fluidPage