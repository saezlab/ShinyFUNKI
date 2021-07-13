# Handle download
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

#handle upload visualisation
uploadResultsObjUI <- function(id, label, title_bs, content) {#tag_bs, 
  ns <- NS(id)
  
  tagList(
    fileInput(ns("upload_Results_funki"),
              label = h5(label,
                         tags$style(type = "text/css", paste0("#", ns("info")," {vertical-align: top;}")),
                         bsButton(ns("info"), label = "", icon = icon("question"), style = "info", size = "extra-small")),
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
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
        userFile()$datapath %>%
        read.delim(., sep = ",") %>%
          dplyr::mutate(dplyr::across(-1, as.numeric)) %>%
          tibble::column_to_rownames(var = "X")
        
      })
      
      # We can run observers in here if we want to
      observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
      })
      
      print(head(dataframe))
      # Return the reactive that yields the data frame
      return(dataframe)
    }
  )    
}