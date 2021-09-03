# Reactive Computations ---------------------------------------------------
doro = uploadResultsObjSever("upload_dorothea_results")

D = reactive({
  if(input$an_dorothea){
    withProgress(message = "Calculate TF activities...", value = 1, {
      if(input$examples){
        organism = "Human"
      }else {organism = input$select_organism}
      dorothea_result = progessDATA(data = expr(),
                                    contrast_data = input$contrast_data,
                                    input$upload_expr,
                                    input$type_analysis) %>%
        run_dorothea(organism = organism, 
                     confidence_level = input$selected_conf_level, 
                     minsize = input$minsize, 
                     method = input$method)
    })
    
  }else{
    dorothea_result = doro()
  }
  return(dorothea_result)
})

# Dynamic widgets / RenderUI ----------------------------------------------
# select contrast/sample
output$select_contrast_dorothea = renderUI({
  req(D())
  choices = colnames(D()) %>%
    str_sort(numeric = T)
  pickerInput(inputId = "select_contrast",
              label = "Select Sample/Contrast",
              choices = choices)
})

# select TFs
output$select_tf = renderUI({
  req(input$select_contrast, D())
    choices = unique(rownames(D()))
    
    default_selected = D() %>%
      data.frame() %>%
      tibble::rownames_to_column(var = "TF") %>%
      dplyr::select(TF, !!as.name(input$select_contrast)) %>%
      dplyr::filter(!!as.name(input$select_contrast) == max(abs(!!as.name(input$select_contrast)))) %>%
      dplyr::select(TF) %>%
      dplyr::pull()
    
    pickerInput(
      inputId = "select_tf",
      label = "Select Transcription Factor",
      choices = choices,
      options = list("live-search" = TRUE),
      selected = default_selected[1]
    )

})

# select number of targets
output$select_top_n_labels = renderUI({
  req(input$select_tf,
      input$select_organism,
      input$selected_conf_level,
      D())
  
    if(input$select_organism == "Human"){
      targets = dorothea_hs
    }else{
      targets = dorothea_mm
    }
    targets = targets %>%
      dplyr::filter(tf == input$select_tf &
                      confidence %in% input$selected_conf_level) %>%
      dplyr::select(target) %>%
      dplyr::filter(target %in% rownames(D())) %>%
      nrow()
    
    sliderInput(
      "select_top_n_labels",
      label = "Number of TF's target genes to display",
      value = dplyr::case_when(targets > 10 ~ 10, targets <= 10 ~ round(targets * (2 / 3))),
      min = 1,
      max = targets,
      step = 1
    )
})

# select top n results
output$select_top_n_hits = renderUI({
  req(D())
    max_tfs = nrow(D())
    sliderInput(
      "select_top_n_hits",
      label = "Number of Transcription Factors to display",
      value = 25,
      min = 1,
      max = max_tfs,
      step = 1
    )
})

#download handler
output$down_doro = renderUI({
  req(D())
  choices = list("DoRothEA scores" = 1, 
                 "Barplot for Sample" = 2, 
                 "Barplot for TF" = 3,
                 "TF's network" = 4)#,
                 # "Heatmap" = 5)
  pickerInput(inputId = "down_doro",
              label = "Select Download",
              choices = choices,
              selected = 1)
})

# Plots ---------------------------------------------------
barplot_nes_reactive_dorothea = reactive ({
  req(D(),
      input$select_contrast,
      input$select_top_n_hits)
    p <- D() %>%
      as.data.frame() %>%
      rownames_to_column(var = "GeneID") %>%
      barplot_nes_dorothea(smpl = input$select_contrast,
                           nHits = input$select_top_n_hits)
})

barplot_tf_reactive = reactive({
  req(input$select_tf, D())
    q <- D() %>%
      data.frame() %>%
      barplot_tf(selTF = input$select_tf)
})

network_tf_reactive = reactive({
  
  req(D(),
      input$select_tf,
      input$select_contrast,
      input$select_top_n_labels,
      input$selected_conf_level)
  
  validate(
    need(expr(), 
         "The plot cannot be showed because the expression file is not included. Please, upload it in the Data and Parameters section.")
  )
  
  if(input$select_organism == "Human"){
    aux = dorothea_hs
  }else{
    aux = dorothea_mm
  }
  
  aux = aux %>%
    dplyr::filter(confidence %in% input$selected_conf_level)
  
  
  plot_network(
    data = expr(), 
    footprint_result = D(),
    regulon = aux,
    sample = input$select_contrast,
    selected_hub = input$select_tf,
    number_targets = input$select_top_n_labels
  )
  
})

heatmap_df_reactive = reactive({
  req(D(),
      input$select_contrast,
      input$select_top_n_hits)
  
  tfs = D() %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "GeneID") %>%
    dplyr::select(GeneID, !!as.name(input$select_contrast)) %>%
    dplyr::mutate(ab = abs(as.numeric(!!as.name(input$select_contrast)))) %>%
    dplyr::top_n(input$select_top_n_hits, wt = ab) %>%
    dplyr::pull(GeneID)
  
  D() %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "GeneID") %>%
    dplyr::filter(GeneID %in% tfs) %>%
    tibble::column_to_rownames(var = "GeneID") %>%
    t() %>% as.data.frame()
  
})

# Render Plots ------------------------------------------------------------

# Bar plot with the TFs for a condition
output$barplot_nes_dorothea = plotly::renderPlotly({
  barplot_nes_reactive_dorothea()
})

# Bar plot of activity for all conditions for a TF
output$tf_bar = plotly::renderPlotly({
  barplot_tf_reactive()
})

# Network of a TF with it's targets
output$tf_network = renderVisNetwork({
  network_tf_reactive()
})

# Heatmap of samples vs TFs
output$heatmap_dorothea = plotly::renderPlotly({
  req(heatmap_df_reactive())
  
  heatmap_df_reactive() %>%
    heatmap_scores()
  
})

# Render Tables -----------------------------------------------------------
# TF-activities
output$dorothea_table = DT::renderDataTable({
  
  if(input$select_organism == "Human"){
    targets = dorothea_hs
  }else{
    targets = dorothea_mm
  }
  
  results_confidence = unique.data.frame(merge(
    targets[, c("tf", "confidence")],
    D() %>%
      data.frame() %>%
      round(digits = 3) %>% 
      rownames_to_column(var = "tf"),
    by = "tf"
  ))
  colnames(results_confidence)[colnames(results_confidence) == "tf"] = "Transcription Factor"
  dorothea_result_matrix = DT::datatable(
    results_confidence,
    # option = list(scrollX = TRUE, autoWidth = T),
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
doro_download = observeEvent({
  input$down_doro
},{
  req(D())
  
  if(input$down_doro == 1){
    a = list(fname = "dorothea_TFactivities_nes.csv",
         cont = function(file){
           write.csv(D(),
                     file,
                     quote = F)
         })
  }else if(input$down_doro == 2){
    a = list(fname = function(){paste0("barplot_dorothea_tfs", input$select_top_n_hits, "_", input$select_contrast, ".png")},
         cont = function(file){ggsave(file, barplot_nes_reactive_dorothea(), device = "png")})
  }else if(input$down_doro == 3){
    a = list(fname = function(){paste0("barplot_dorothea_samples_", input$select_tf, ".png")},
         cont = function(file){ggsave(file, barplot_tf_reactive(), device = "png")})
  }else if(input$down_doro == 4){
    a = list(fname = paste0("network_dorothea_targets_", input$select_tf, "_", input$select_contrast, ".png"),
         cont = function(file){
           visSave(network_tf_reactive(), "temp.html")
           webshot::webshot("temp.html", zoom = 2, file = file)
           file.remove("temp.html")})
  }else if(input$down_doro == 5){
    a = list(fname = function(){paste0("heatmap_dorothea_sample_", input$select_tf, "_view.png")},
             cont = function(file){
               plotly::orca(heatmap_dorothea(), file)
               }
             )
  }
  
  downloadObjSever("download_dorothea", filename = a$fname, content = a$cont)
})