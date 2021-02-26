# Show expression data 
expr = reactive({
  if (input$example_data) {
    shinyjs::disable("upload_expr")
    shinyjs::disable("select_organism")
    expDATA = read_csv("data/examples/example_data.csv") 
  } else {
    shinyjs::enable("upload_expr")
    shinyjs::enable("select_organism")
    inFile = input$upload_expr$datapath
    
    if (is.null(inFile)){
      return(NULL)
    } else{
      expDATA = read_csv(inFile)
    }
  }
  return(expDATA)
})

output$expr = DT::renderDataTable({
  if(!is.null(expr())){
    DT::datatable(expr(), option = list(autoWidth = TRUE, pageLength = 5)) %>%
      formatSignif(which(map_lgl(expr(), is.numeric)))
  }
})  

# if a file is uploaded, the calculation button in enabled
observeEvent({
  input$upload_expr
  input$example_data}, {
    toggleState("run_dorothea",
                input$example_data == T | !is.null(input$upload_expr))
    toggleState("run_progeny",
                input$example_data == T | !is.null(input$upload_expr))
    toggleState("run_carnival",
                input$example_data == T | !is.null(input$upload_pprot))
  })
