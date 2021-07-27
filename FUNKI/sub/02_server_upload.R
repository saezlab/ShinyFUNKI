# Show expression data 
expr = reactive({
  toggleState("upload_expr", 
              !input$example_data & !input$phospho_data & !input$contrast_data)
  toggleState("select_organism", 
              !input$example_data & !input$phospho_data & !input$contrast_data)
  toggleState("type_analysis", 
              !input$example_data & !input$phospho_data & !input$contrast_data)
  toggleState("phospho_data", 
              !input$example_data & !input$contrast_data)
  toggleState("example_data", 
              !input$phospho_data & !input$contrast_data)
  toggleState("contrast_data", 
              !input$phospho_data & !input$example_data)
  
  if(input$example_data){
    expDATA = read_csv("data/examples/example_data.csv") %>% 
      dplyr::select(contrast, gene, logFC) %>% 
      tidyr::pivot_wider(names_from = contrast, values_from = logFC) %>%
      data.frame() %>%
      tibble::column_to_rownames(var = "gene")
      
  }else if(input$phospho_data){
    expDATA = read.csv("data/examples/phospho_data.csv", row.names = 1, header = TRUE)
  }else if(input$contrast_data){
    expDATA = read.delim("data/examples/contrast_cosmos.csv", header = T, sep  =  ',') %>%
      tibble() %>%
      dplyr::mutate(ID = as.character(ID)) %>%
      arrange(HGNC, -adj.P.Val) %>%
      filter(duplicated(HGNC) == FALSE) %>%
      tibble::column_to_rownames(var = "HGNC")

 }else {
    
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
  shiny::req(expr())
  DT::datatable(expr(), option = list(autoWidth = TRUE, pageLength = 5)) %>%
    formatSignif(which(map_lgl(expr(), is.numeric)))
})

# if a file is uploaded, the calculation button in enabled
observeEvent({
  input$upload_expr
  input$example_data
  input$phospho_data
  input$dorothea
  input$contrast_data}, {
    toggleState("an_dorothea",
                input$example_data | input$contrast_data | !is.null(input$upload_expr))
    toggleState("an_progeny",
                input$example_data | input$contrast_data | !is.null(input$upload_expr))
    toggleState("an_carnival",
                input$example_data | input$contrast_data | !is.null(input$upload_expr) | input$dorothea == "up")
    toggleState("an_cosmos",
                input$contrast_data | !is.null(input$upload_expr))
    toggleState("an_kinact",
                input$phospho_data | !is.null(input$upload_expr))
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

observeEvent(input$an_cosmos, {
  updateTabsetPanel(session, inputId = "menu",
                    selected = "COSMOS")
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