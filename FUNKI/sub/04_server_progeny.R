# Reactive Computations ---------------------------------------------------

prog = uploadResultsObjSever("upload_progeny_results")

P = reactive({

  if(input$an_progeny){
    showModal(modalDialog("Running PROGENy", footer = NULL))

      if(input$examples){
        organism = "Human"
      }else {organism = input$select_organism}
      
      data = progessDATA(data = expr(),
                         contrast_data = input$contrast_data,
                         upload_expr = input$upload_expr,
                         type_analysis = input$type_analysis,
                         gene_id_type = input$gene_id_type,
                         running_method = "progeny")
      prog_result = data %>%
        run_progeny(organism = organism,
                    top = input$top,
                    perm = input$perm)

      if(ncol(data) == 1){rownames(prog_result) = colnames(data)}
    removeModal()

  }else{
    prog_result = prog()
  }

  return(prog_result)
})

# Dynamic widgets / RenderUI ----------------------------------------------

# select contrast/sample
output$select_contrast_progeny = renderUI({
  req(P())
    choices = rownames(P()) 
    
    pickerInput(inputId = "select_contrast_progeny",
                label = "Select Sample/Contrast",
                choices = choices,
                selected = choices[1])
})

# select pathway
output$select_pathway = renderUI({
  req(P(), input$select_contrast_progeny)
    choices = colnames(P()) %>%
      str_sort(numeric = T)
    
    default_selected = P() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "pathway") %>%
      dplyr::select(pathway, !!as.name(input$select_contrast_progeny)) %>%
      dplyr::filter(!!as.name(input$select_contrast_progeny) == max(abs(!!as.name(input$select_contrast_progeny)))) %>%
      dplyr::select(pathway) %>%
      dplyr::pull()
    
    pickerInput(
      inputId = "select_pathway",
      label = "Select Pathway",
      choices = choices,
      options = list("live-search" = TRUE),
      selected = default_selected[1]
    )

})

#download handler
output$down_progeny = renderUI({
  req(P())
  choices = list("PROGENy scores" = 1, 
                 "Barplot for Sample" = 2, 
                 "Scatterplot" = 3)#,
                 # "Heatmap" = 4)
  pickerInput(inputId = "down_proge",
              label = "Select Download",
              choices = choices,
              selected = 1)
})

# Bar plot with the TFs for a condition------------------------------------

barplot_nes_reactive_progeny = reactive ({
  req(P(), input$select_contrast_progeny)
  P() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "pathways") %>%
      barplot_nes_progeny(smpl = input$select_contrast_progeny)
  
})

scatter_reactive = reactive({
  req(P(), input$select_pathway, input$select_contrast_progeny)
  
  validate(
    need(expr(), 
         "The plot cannot be showed because the expression file is not included. Please, upload it in the Data and Parameters section.")
  )

    if (input$example_data){
      organism = "Human"
    }else {organism = input$select_organism}
    
    prog_matrix <- progeny::getModel(organism = organism, top =  input$top) %>%
      tibble::rownames_to_column("GeneID") %>%
      dplyr::select(GeneID, input$select_pathway)
    
    inputProgeny_df <- progessDATA(data = expr(),
                                   contrast_data = input$contrast_data,
                                   upload_expr = input$upload_expr,
                                   type_analysis = input$type_analysis,
                                   gene_id_type = input$gene_id_type,
                                   running_method = "progeny") %>%
      as.data.frame() %>%
      dplyr::select(input$select_contrast_progeny) %>%
      tibble::rownames_to_column("GeneID")
    
    title = paste0(
      "Weights of ",    
      input$select_pathway,
      " for ",
      input$select_contrast_progeny
    )
    
    scat_plots <- scater_pathway(df = inputProgeny_df,
                                 weight_matrix = prog_matrix,
                                 title = title)

})

# Render Tables -----------------------------------------------------------
# Progent-activities
output$progeny_table = DT::renderDataTable({
  req(P())
  
  results_progeny = P() %>%
    t() %>%
    as.data.frame() %>%
    round(digits = 3) %>%
    rownames_to_column(var = "Pathways")
  
  result_matrix = DT::datatable(
    results_progeny,
    extensions = "Buttons",
    filter = "top",
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      pageLength = 14,
      dom = 'tB',
      buttons = c('csv', 'excel'))
  )
  
})

# Render Plots ------------------------------------------------------------

# Bar plot with the TFs for a condition
output$barplot_progeny = plotly::renderPlotly({
  barplot_nes_reactive_progeny()
})

# Bar plot of activity for all conditions for a TF
output$scatter = renderPlot({
  print(scatter_reactive())
})

# Heatmap for all samples and pathways
output$heatmap_progeny = plotly::renderPlotly({
  req(P())
    P() %>%
      heatmap_scores()
})

# Download Handler --------------------------------------------------------
progeny_download = observeEvent({
  input$down_proge
},{
  req(P())
  
  if(input$down_proge == 1){
    a = list(fname = "progeny_pathwayScores_nes.csv",
             cont = function(file){
               write.csv(P(),
                         file,
                         quote = F)
             })
  }else if(input$down_proge == 2){
    a = list(fname = function(){paste0("barplot_progeny_pathways", "_", input$select_contrast_progeny, ".png")},
             cont = function(file){ggsave(file, barplot_nes_reactive_progeny(), device = "png")})
  }else if(input$down_proge == 3){
    a = list(fname = function(){paste0("scatterplot_progeny_", input$select_pathway, "_", input$select_contrast_progeny, ".png")},
             cont = function(file){ggsave(file, scatter_reactive(), device = "png")})
  }else if(input$down_proge == 4){
    a = list(fname = function(){paste0("heatmap_progeny.png")},
             cont = function(file){
               ggsave(file, heatmap_scores(df = P()), device = "png")
             })
  }
  
  downloadObjSever("download_progeny", filename = a$fname, content = a$cont)
  downloadReportSever("progeny_report", 
                      fname = "report_progeny.html",
                      report = "progeny_report.Rmd",
                      parameters = list(
                        top = input$top,
                        perm = input$perm,
                        organism = input$select_organism,
                        selected_sample = input$select_contrast_progeny,
                        selected_pathway = input$select_pathway,
                        sample_plot = barplot_nes_reactive_progeny(),
                        heatmap_plot = heatmap_scores(P()),
                        scatter_plot = scatter_reactive()
                      ))
})