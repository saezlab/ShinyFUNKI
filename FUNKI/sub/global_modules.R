# Handle download ---------------------------
downloadObjUI <- function(id) {
  ns <- NS(id)

  downloadButton(ns("funki_download"), label = "Download")
}

downloadObjSever <- function(id, filename, content) {
  moduleServer(
    id,
    function(input, output, session){
      output$funki_download <- downloadHandler(
        filename = filename,
        content = content
      )
    })
}

# Handle upload resuts widget and server -------------
uploadResultsObjUI <- function(id, label, title_bs, content) {#tag_bs, 
  ns <- NS(id)
  
  tagList(
    fileInput(ns("upload_Results_funki"),
              label = h5(label,
                         tags$style(type = "text/css", paste0("#", ns("info")," {vertical-align: top;}")),
                         bsButton(ns("info"), label = "", icon = icon("question"), style = "info", size = "extra-small")),
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".rds")
    ),
    bsPopover(id = ns("info"),#tag_bs, 
              title = title_bs,
              content = content,
              placement = "right",
              trigger = "click",
              options = list(container = "body")
    )
  )
  
}

uploadResultsObjSever <- function(id) {
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      # The selected file, if any
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$upload_Results_funki, message = FALSE))
        input$upload_Results_funki
      })
      
      # The user's data, parsed into a data frame
      dataframe <- reactive({
        if(!is.null(userFile()$name)){
          n = grepl("\\.rds$", userFile()$name)
        }
        if(n){
          userFile()$datapath %>% readRDS(.)
        }else{
          userFile()$datapath %>%
            read.delim(., sep = ",") %>%
            dplyr::mutate(dplyr::across(-1, as.numeric)) %>%
            tibble::column_to_rownames(var = "X")
          
        }
        
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )    
}

# Handle and server for download report ---------------------------
downloadReportUI <- function(id) {
  ns <- NS(id)
  
  downloadButton(ns("report"), label = "Generate report")
}

downloadReportSever <- function(id, fname, parameters, report) {
  moduleServer(
    id,
    function(input, output, session){
      output$report <- downloadHandler(
        # HTML report
        filename = fname,
        content = function(fname) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), report)
          file.copy(paste0("data/reports/", report), tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          # params <- parameters #list(n = input$slider)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, 
                            output_file = fname,
                            params = parameters,
                            envir = new.env(parent = globalenv())
          )
      })
    }
  )
}

withBusyIndicatorCSS <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: orange ;
}
"

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Warning: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}
