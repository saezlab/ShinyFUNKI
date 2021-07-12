
downloadObjUI <- function(id) {
  ns <- NS(id)

  downloadButton(ns("funki_download"), label = "Download")
}

downloadObj <- function(input, output, session, filename, content) {
  
  output$funki_download <- downloadHandler(
    filename = filename,
    content = content
  )
}