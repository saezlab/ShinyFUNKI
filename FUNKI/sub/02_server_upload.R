# Show expression data 
expr = reactive({
  if (any(input$example_data | input$phospho_data)){
    shinyjs::disable("upload_expr")
    shinyjs::disable("select_organism")
    if(input$example_data){
      expDATA = read_csv("data/examples/example_data.csv") %>% 
        dplyr::select(contrast, gene, logFC) %>% 
        tidyr::pivot_wider(names_from = contrast, values_from = logFC) %>%
        data.frame() %>%
        tibble::column_to_rownames(var = "gene")
      
    }
    if(input$phospho_data){
      expDATA = read.csv("data/examples/phospho_data.csv", row.names = 1, header = TRUE)
    }
  } else {
    shinyjs::enable("upload_expr")
    shinyjs::enable("select_organism")
    inFile = input$upload_expr$datapath
    
    if (is.null(inFile)){
      return(NULL)
    } else{
      expDATA = read.csv(inFile, row.names = 1, header = TRUE)
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
  input$example_data
  input$phospho_data}, {
    toggleState("select_organism",
                input$example_data == T | !is.null(input$upload_expr))
    toggleState("upload_expr",
                input$example_data == T )
    toggleState("an_dorothea",
                all(input$example_data == T & input$phospho_data == F) | !is.null(input$upload_expr))
    toggleState("an_progeny",
                all(input$example_data == T & input$phospho_data == F) | !is.null(input$upload_expr))
    toggleState("an_carnival",
                all(input$example_data == T & input$phospho_data == F) | !is.null(input$upload_expr) | !is.null(input$upload_tfs))
    toggleState("an_kinact",
                all(input$example_data == F & input$phospho_data == T) | !is.null(input$upload_expr))
    
  })

# jump to visualise results
observeEvent(input$an_dorothea, {
  updateTabsetPanel(session, inputId = "menu",
                    selected = "DoRothEA")
})

observeEvent(input$an_progeny, {
  updateTabsetPanel(session, inputId = "menu",
                    selected = "PROGENy")
})

observeEvent(input$an_carnival, {
  updateTabsetPanel(session, inputId = "menu",
                    selected = "CARNIVAL")
})

observeEvent(input$an_kinact, {
  updateTabsetPanel(session, inputId = "menu",
                    selected = "KinAct")
})

#get path for CARNIVAL solver
volumes <- getVolumes()
observe({
  shinyFileChoose(input, 'solverPath', roots = volumes, session = session) 
  solverpath <<- paste( unlist(unname(input$solverPath[1])), collapse = "/")
})

#Select sample for CARNIVAL analysis
# select contrast/sample
output$select_sample_carnival = renderUI({
  if (!is.null(expr())) {
    choices = colnames(expr()) 
    
    pickerInput(inputId = "select_sample_carnival",
                label = "Select Sample/Contrast",
                choices = choices,
                selected = choices[1])
  }
})
