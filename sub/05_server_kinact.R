K = eventReactive(input$an_kinact, {

      withProgress(message = "Calculate KinAct matrix", value = 1, {
        kinact_result = run_kinact(data = expr(), 
                                   organism = "Human", 
                                   minsize = 5, 
                                   method = 'none')
          
        })
})

# Dynamic widgets / RenderUI ----------------------------------------------

# select contrast/sample
output$select_contrast_kinact = renderUI({
  if (!is.null(K())) {
    choices = colnames(K()) %>%
      str_sort(numeric = T)
    pickerInput(inputId = "select_contrast_kinact",
                label = "Select Sample/Contrast",
                choices = choices)
  }
})

# select kinase
output$select_kinase = renderUI({
  if (!is.null(input$select_contrast_kinact)) {
    choices = unique(rownames(K()))
    
    default_selected = K() %>%
      data.frame() %>%
      tibble::rownames_to_column(var = "Kinase") %>%
      dplyr::select(Kinase, !!as.name(input$select_contrast_kinact)) %>%
      dplyr::filter(!!as.name(input$select_contrast) == max(abs(!!as.name(input$select_contrast_kinact)))) %>%
      dplyr::select(Kinase) %>%
      dplyr::pull()
    
    pickerInput(
      inputId = "select_tf",
      label = "Select Kinase",
      choices = choices,
      options = list("live-search" = TRUE),
      selected = default_selected[1]
    )
    
  }
})

# select number of targets for kinase
output$select_top_targets = renderUI({
  if (!is.null(input$select_kinase)) {

    targets = kinact_regulon_human %>%
      dplyr::filter(kinase == input$select_kinase) %>%
      dplyr::select(target) %>%
      dplyr::filter(target %in% rownames(K())) %>%
      nrow()
    
    sliderInput(
      "select_top_targets",
      label = "Number of targets to display",
      value = dplyr::case_when(targets > 10 ~ 10, targets <= 10 ~ round(targets * (2 / 3))),
      min = 1,
      max = targets,
      step = 1
    )
  }
})

# select top kinases
output$select_top_kinases = renderUI({
  if (!is.null(K())) {
    max_kin = nrow(K())
    sliderInput(
      "select_top_kinases",
      label = "Number of Kinases to display",
      value = 25,
      min = 1,
      max = max_kin,
      step = 1
    )
  }
})

# Bar plot with the TFs for a condition---------------------------------------------------
barplot_nes_reactive_kinact = reactive ({
  if (!is.null(input$select_contrast) &
      !is.null(input$select_top_kinases)) {
    p <- K() %>%
      as.data.frame() %>%
      rownames_to_column(var = "GeneID") %>%
      barplot_nes_dorothea(smpl = input$select_contrast_kinact,
                           nHits = input$select_top_kinases)
  }
  
})

barplot_kinase_reactive = reactive({
  if (!is.null(input$select_kinase)) {
    q <- K() %>%
      data.frame() %>%
      barplot_tf(selTF = input$select_kinase)
    
  }
  
})

network_kinase_reactive = reactive({
  
  if (!is.null(input$select_kinase) &
      !is.null(input$select_contrast_kinact)) {
    aux = kinact_regulon_human %>%
      dplyr::filter(kinase == input$select_kinase)
    
    nodes = merge.data.frame(
      aux %>%
        dplyr::select(target),
      K() %>%
        as.data.frame() %>%
        dplyr::select(input$select_contrast_kinact) %>%
        tibble::rownames_to_column("target"),
      by = "target"
    )
    
    nodes = nodes[order(abs(nodes[, input$select_contrast_kinact]), decreasing = T), ]
    
    if (input$select_top_targets <= nrow(nodes)) {
      nodes = nodes[1:input$select_top_targets, ]
    }
    
    nodes = tibble(
      rbind.data.frame(
        nodes,
        K() %>%
          data.frame() %>%
          tibble::rownames_to_column("target") %>%
          dplyr::filter(target == input$select_kinase) %>%
          dplyr::select(target, input$select_contrast_kinact)
      )
    )
    
    nodes$regulation = nodes[, input$select_contrast_kinact]
    
    nodes = nodes %>%
      mutate(
        regulation = dplyr::case_when(
          regulation >= 0 ~ "upregulated",
          regulation < 0 ~ "downregulated"
        )
      )
    
    gtitle = paste0("Sample/Contrast: ",
                    input$select_contrast_kinact,
                    "; Kinase: ",
                    input$select_kinase)
    
    plot_network(
      network = aux %>% dplyr::select(tf, mor, target),
      nodes = nodes,
      title = gtitle
    )
    
  }
  
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
output$kinase_network = renderPlot({
  print(barplot_kinase_reactive())
})

# Heatmap of samples vs TFs
output$heatmap_kinase = renderPlot({
  if(!is.null(K())){
    K() %>% t() %>% data.frame() %>%
      heatmap_scores()
  }
})

# Render Tables -----------------------------------------------------------
# TF-activities
output$kinase_table = DT::renderDataTable({
  
  results = K() %>%
      data.frame() %>%
      round(digits = 3) %>% 
      rownames_to_column(var = "Kinase")
  
  kinase_result_matrix = DT::datatable(
    results,
    option = list(scrollX = TRUE, autoWidth = T),
    filter = "top"
  )
  
})

# Download Handler --------------------------------------------------------

# All in a tar
output$download_kinact_analysis = downloadHandler(
  filename = "footprint_kinact_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_kinact_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    fnames = c(
      paste0("barplot_kinact_", input$select_contrast_kinact, ".png"),
      paste0("barplot_samples_", input$select_kinase, ".png"),
      paste0("network_", input$select_contrast_kinact, "_", input$select_kinase, ".png")
    )
    
    ggsave(file.path(fdir, fnames[1]), barplot_nes_reactive_dorothea(), device = "png")
    ggsave(file.path(fdir, fnames[2]), barplot_kinase_reactive(), device = "png")
    ggsave(file.path(fdir, fnames[3]), network_kinase_reactive(), device = "png")
    write.csv(K(),
              file.path(fdir, "kinasesActivities_nes.csv"),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)
