# Reactive Computations ---------------------------------------------------
D = eventReactive({
  input$an_dorothea
}, {
  
  if (!is.null(input$selected_conf_level)) {

    withProgress(message = "Calculate TF activities...", value = 1, {
      
      if (input$example_data){
        organism = "Human"
      }else {organism = input$select_organism}
      
      dorothea_result = run_dorothea(dorothea_matrix = expr(), 
                                     organism = organism, 
                                     confidence_level = input$selected_conf_level, 
                                     minsize = input$minsize, 
                                     method = input$method)
    })
  }

})

# Dynamic widgets / RenderUI ----------------------------------------------

# select contrast/sample
output$select_contrast_dorothea = renderUI({
  if (!is.null(D())) {
    choices = colnames(D()) %>%
      str_sort(numeric = T)
    pickerInput(inputId = "select_contrast",
                label = "Select Sample/Contrast",
                choices = choices)
  }
})

# select TFs
output$select_tf = renderUI({
  if (!is.null(input$select_contrast)) {
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
    
  }
})

# select number of targets
output$select_top_n_labels = renderUI({
  if (!is.null(input$select_tf)) {
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
      label = "Number of targets to display",
      value = dplyr::case_when(targets > 10 ~ 10, targets <= 10 ~ round(targets * (2 / 3))),
      min = 1,
      max = targets,
      step = 1
    )
  }
})

# select top n results
output$select_top_n_hits = renderUI({
  if (!is.null(D())) {
    max_tfs = nrow(D())
    sliderInput(
      "select_top_n_hits",
      label = "Number of Transcription Factors to display",
      value = 25,
      min = 1,
      max = max_tfs,
      step = 1
    )
  }
})

# Bar plot with the TFs for a condition---------------------------------------------------
barplot_nes_reactive_dorothea = reactive ({
  if (!is.null(input$select_contrast) &
      !is.null(input$select_top_n_hits)) {
    p <- D() %>%
      as.data.frame() %>%
      rownames_to_column(var = "GeneID") %>%
      barplot_nes_dorothea(smpl = input$select_contrast,
                  nHits = input$select_top_n_hits)
  }
  
})

barplot_tf_reactive = reactive({
  if (!is.null(input$select_tf)) {
    q <- D() %>%
      data.frame() %>%
      barplot_tf(selTF = input$select_tf)
    
  }
  
})

network_tf_reactive = reactive({
  
  if(input$select_organism == "Human"){
    aux = dorothea_hs
  }else{
    aux = dorothea_mm
  }
  
  if (!is.null(input$select_tf) &
      !is.null(input$select_contrast)) {
    aux = aux %>%
      dplyr::filter(tf == input$select_tf &
                      confidence %in% input$selected_conf_level)
    
    nodes = merge.data.frame(
      aux %>%
        dplyr::select(target),
      D() %>%
      as.data.frame() %>%
        dplyr::select(input$select_contrast) %>%
        tibble::rownames_to_column("target"),
      by = "target"
    )
    
    nodes = nodes[order(abs(nodes[, input$select_contrast]), decreasing = T), ]
    
    if (input$select_top_n_labels <= nrow(nodes)) {
      nodes = nodes[1:input$select_top_n_labels, ]
    }
    
    nodes = tibble(
      rbind.data.frame(
        nodes,
        D() %>%
          data.frame() %>%
          tibble::rownames_to_column("target") %>%
          dplyr::filter(target == input$select_tf) %>%
          dplyr::select(target, input$select_contrast)
      )
    )
    
    nodes$regulation = nodes[, input$select_contrast]
    
    nodes = nodes %>%
      mutate(
        regulation = dplyr::case_when(
          regulation >= 0 ~ "upregulated",
          regulation < 0 ~ "downregulated"
        )
      )
    
    gtitle = paste0("Sample/Contrast: ",
                    input$select_contrast,
                    "; TF: ",
                    input$select_tf)
    
    plot_network(
      network = aux %>% dplyr::select(tf, mor, target),
      nodes = nodes,
      title = gtitle
    )
    
  }
  
  
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
output$tf_network = renderPlot({
  print(network_tf_reactive())
})


# Heatmap of samples vs TFs
output$heatmap_dorothea = renderPlot({
  if(!is.null(D())){
    D() %>% t() %>% data.frame() %>%
      heatmap_scores()
  }
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
    option = list(scrollX = TRUE, autoWidth = T),
    filter = "top"
  )
  
})

# Download Handler --------------------------------------------------------

# All in a tar
output$download_dorothea_analysis = downloadHandler(
  filename = "footprint_dorothea_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_dorothea_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    fnames = c(
      paste0("barplot_tfs_", input$select_contrast, ".png"),
      paste0("barplot_samples_", input$select_tf, ".png"),
      paste0("network_", input$select_contrast, "_", input$select_tf, ".png")
    )
    
    ggsave(file.path(fdir, fnames[1]), barplot_nes_reactive_dorothea(), device = "png")
    ggsave(file.path(fdir, fnames[2]), barplot_tf_reactive(), device = "png")
    ggsave(file.path(fdir, fnames[3]), network_tf_reactive(), device = "png")
    write.csv(D(),
              file.path(fdir, "TFactivities_nes.csv"),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)
