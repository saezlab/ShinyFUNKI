
# if a file is uploaded, the calculation button in enabled
observeEvent({
  input$upload_expr
  input$take_example_data}, {
    toggleState("run_dorothea", 
                input$take_example_data == T | !is.null(input$upload_expr))
    toggleState("run_progeny", 
                input$take_example_data == T | !is.null(input$upload_expr))
  })



# models
interactome = reactive({
  if (input$take_example_data == F & quality_vs_coverage() == "Quality") {
    switch(input$select_organism,
           "Homo sapiens" = dorothea_regulon_human_v1,
           "Mus musculus" = dorothea_regulon_mouse_v1
    )
  } else if (input$take_example_data == F & quality_vs_coverage() == "Coverage") {
    switch(input$select_organism,
           "Homo sapiens" = dorothea_regulon_human_coverage_v1,
           "Mus musculus" = dorothea_regulon_mouse_coverage_v1
    )
  } else if (input$take_example_data == T & quality_vs_coverage() == "Quality") {
    dorothea_regulon_mouse_v1
  } else if (input$take_example_data == T & quality_vs_coverage() == "Coverage") {
    dorothea_regulon_mouse_coverage_v1
  }
})

matrix = reactive({
  if (input$take_example_data == F) {
  switch(input$select_organism,
         "Homo sapiens" = progeny_matrix_human_v1,
         "Mus musculus" = progeny_matrix_mouse_v1
  )
  } else {
    progeny_matrix_mouse_v1
  }
})


kinact_interactome = reactive({
  kinact_regulon_human
})


# data
expr = reactive({
  if (input$take_example_data == F) {
    shinyjs::enable("upload_expr")
    shinyjs::enable("select_organism")
    inFile = input$upload_expr
      if (is.null(inFile)){
        return(NULL)
      }
    read_csv(inFile$datapath)
  } else {
    shinyjs::disable("upload_expr")
    shinyjs::disable("select_organism")
    read_csv("data/limma_result.csv") 
  }
})

extended_expr = reactive({
  if (!is.null(dorothea_selected_padj_cutoff())) {
    res = expr() %>%
      mutate(effect = case_when(adj.p.value <= dorothea_selected_padj_cutoff() & logFC > 0 ~ "upregulated",
                                adj.p.value <= dorothea_selected_padj_cutoff() & logFC < 0 ~ "downregulated",
                                TRUE ~ "not regulated"),
             importance = abs(logFC * -log10(adj.p.value)))
    print(res)
    return(res)
  }
})

# data
ppomics = reactive({
  if (input$take_example_data == F) {
    shinyjs::enable("upload_expr")
    #shinyjs::enable("select_organism")
    inFile = input$upload_pprot
    if (is.null(inFile)){
      return(NULL)
    }
    read_csv(inFile$datapath)
  } else {
    shinyjs::disable("upload_pprot")
    #shinyjs::disable("select_organism")
    read_csv("data/phospho_clean.csv") 
  }
})

extended_ppomics = reactive({
  if (!is.null(kinact_selected_padj_cutoff())) {
    res = ppomics() %>%
      mutate(effect = case_when(adj.p.value <= kinact_selected_padj_cutoff() & logFC > 0 ~ "upregulated",
                                adj.p.value <= kinact_selected_padj_cutoff() & logFC < 0 ~ "downregulated",
                                TRUE ~ "not regulated"),
             importance = abs(logFC * -log10(adj.p.value)))
    print(res)
    return(res)
  }
})


output$expr = DT::renderDataTable({
  if (!is.null(expr())) {
    DT::datatable(expr(), option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
      formatSignif(which(map_lgl(expr(), is.numeric)))
  }

})

output$ppomics = DT::renderDataTable({
  if (!is.null(ppomics())) {
    DT::datatable(ppomics(), option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
      formatSignif(which(map_lgl(ppomics(), is.numeric)))
  }
})
