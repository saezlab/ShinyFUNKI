# Reactive Computations ---------------------------------------------------
kin = uploadResultsObjSever("upload_kinact_results")

K = reactive({
  if(input$an_kinact){
    showModal(modalDialog("Running KinAct", footer = NULL))
      
      kinact_result = progessDATA(data = expr(),
                                  contrast_data = F,
                                  upload_expr = input$upload_expr,
                                  type_analysis = NULL,
                                  gene_id_type = input$gene_id_type,
                                  running_method = "kinact") %>% 
        run_kinact(organism = "Human",
                   minsize = input$minsize_kinact,
                   method = input$method_kinact)
      
      
      print(head(kinact_result))
      
    removeModal()
    
  }else{
    kinact_result = kin()
  }
  return(kinact_result)
})

# Dynamic widgets / RenderUI ----------------------------------------------
# select contrast/sample
output$select_contrast_kinact = renderUI({
  req(K())
  
  choices = colnames(K()) %>%
      str_sort(numeric = T)
  
  pickerInput(inputId = "select_contrast_kinact",
              label = "Select Sample/Contrast",
              choices = choices)
})

# select kinase
output$select_kinase = renderUI({
  req(input$select_contrast_kinact, K())
  
  choices = unique(rownames(K()))
    
  default_selected = K() %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "Kinase") %>%
    dplyr::select(Kinase, !!as.name(input$select_contrast_kinact)) %>%
    dplyr::filter(!!as.name(input$select_contrast_kinact) == max(abs(!!as.name(input$select_contrast_kinact)))) %>%
    dplyr::select(Kinase) %>%
    dplyr::pull()
    
  pickerInput(
    inputId = "select_kinase",
    label = "Select Kinase",
    choices = choices,
    options = list("live-search" = TRUE),
    selected = default_selected[1]
    )

})

# select number of targets for kinase
output$select_top_targets = renderUI({
  
  req(input$select_kinase,
      K())

  targets = kinact_regulon_human %>%
    dplyr::filter(kinase == input$select_kinase) %>%
    dplyr::select(target) %>%
    dplyr::filter(target %in% expr()$ID_site) %>%
    nrow()
    
  sliderInput(
    "select_top_targets",
    label = "Number of Kinase substrates to display",
    value = dplyr::case_when(targets > 10 ~ 10, targets <= 10 ~ round(targets * (2 / 3))),
    min = 1,
    max = targets,
    step = 1
  )
})

# select top kinases
output$select_top_kinases = renderUI({
  req(K())
  max_kin = nrow(K())
  
  sliderInput(
    "select_top_kinases",
    label = "Number of Kinases to display",
    value = 25,
    min = 1,
    max = max_kin,
    step = 1
  )
})

#download handler
output$down_kinact = renderUI({
  req(K())
  choices = list("KinAct scores" = 1, 
                 "Barplot for Sample" = 2, 
                 "Barplot for Kinase" = 3,
                 "Kinase's network" = 4)#,
  # "Heatmap" = 5)
  pickerInput(inputId = "down_kinact",
              label = "Select Download",
              choices = choices,
              selected = 1)
})

# Plots ---------------------------------------------------
barplot_nes_reactive_kinact = reactive ({
  req(K(),
      input$select_contrast_kinact,
      input$select_top_kinases)
  K() %>%
    as.data.frame() %>%
    rownames_to_column(var = "GeneID") %>%
    barplot_nes_dorothea(smpl = input$select_contrast_kinact,
                         nHits = input$select_top_kinases)
  
})

barplot_kinase_reactive = reactive({
  req(input$select_kinase, K())
  K() %>%
    data.frame() %>%
    barplot_tf(selTF = input$select_kinase)
})

network_kinase_reactive = reactive({
  
  req(K(),
      input$select_kinase,
      input$select_contrast_kinact,
      input$select_top_targets)
  
  validate(
    need(expr(), 
         "The plot cannot be showed because the expression file is not included. Please, upload it in the Data and Parameters section.")
  )
  
  progessDATA(data = expr(),
              contrast_data = input$contrast_data,
              upload_expr = input$upload_expr,
              type_analysis = input$type_analysis,
              gene_id_type = input$gene_id_type,
              running_method = "kinact") %>%
  plot_network(.,
    footprint_result = K(),
    regulon = kinact_regulon_human,
    sample = input$select_contrast_kinact,
    selected_hub = input$select_kinase,
    number_targets = input$select_top_targets
  )
  
})

# Render Plots ------------------------------------------------------------

# Bar plot with the TFs for a condition
output$barplot_nes_kinase = plotly::renderPlotly({
  barplot_nes_reactive_kinact()
})

# Bar plot of activity for all conditions for a TF
output$kinase_bar = plotly::renderPlotly({
  barplot_kinase_reactive()
})

# Network of a TF with it's targets
output$kinase_network = renderVisNetwork({
  network_kinase_reactive()
})

# Heatmap of samples vs TFs
output$heatmap_kinase =  plotly::renderPlotly({
  req(K())
    K() %>% t() %>% data.frame() %>%
      heatmap_scores()
})

# Render Tables -----------------------------------------------------------
output$kinase_table = DT::renderDataTable({
  
  results = K() %>%
      data.frame() %>%
      round(digits = 3) %>% 
      rownames_to_column(var = "Kinase")
  
  kinase_result_matrix = DT::datatable(
    results,
    filter = "top",
    extensions = "Buttons",
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'tB',
      buttons = c('csv', 'excel'))
  )

})

# Download Handler --------------------------------------------------------
kinact_download = observeEvent({
  input$down_kinact
},{
  req(K())
  
  if(input$down_kinact == 1){
    a = list(fname = "kinact_Kinase_activities_nes.csv",
             cont = function(file){
               write.csv(K(),
                         file,
                         quote = F)
             })
  }else if(input$down_kinact == 2){
    a = list(fname = function(){paste0("barplot_kinact_kinases", input$select_top_targets, "_", input$select_contrast_kinact, ".png")},
             cont = function(file){ggsave(file, barplot_nes_reactive_kinact(), device = "png")})
  }else if(input$down_kinact == 3){
    a = list(fname = function(){paste0("barplot_kinact_samples_", input$select_kinase, ".png")},
             cont = function(file){ggsave(file, barplot_kinase_reactive(), device = "png")})
  }else if(input$down_kinact == 4){
    a = list(fname = paste0("network_kinact_substract_", input$select_kinase, "_", input$select_contrast_kinact, ".png"),
             cont = function(file){
               visSave(network_kinase_reactive(), "temp.html")
               webshot::webshot("temp.html", zoom = 2, file = file)
               file.remove("temp.html")})
  }else if(input$down_kinact == 5){
    a = list(fname = function(){paste0("heatmap_kinact_sample_", input$select_kinase, "_view.png")},
             cont = function(file){
               plotly::orca(heatmap_kinase(), file)
             }
    )
  }
  
  downloadObjSever("download_kinact", filename = a$fname, content = a$cont)
})