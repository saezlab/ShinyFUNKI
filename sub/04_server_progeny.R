# Reactive Computations ---------------------------------------------------
P = eventReactive({
  input$an_progeny
  input$select_organism
  input$perm
  input$top
  },{
    if(!is.null(input$an_progeny)){
      withProgress(message = "Calculate PROGENy matrix", value = 1, {
        expr() %>%
          run_progeny(organism = input$select_organism, 
                      top = input$perm, 
                      perm = input$top)
      })
    }
    
})

# Dynamic widgets / RenderUI ----------------------------------------------

# select contrast/sample
output$select_contrast_progeny = renderUI({
  if (!is.null(P())) {
    choices = rownames(P()) 
    
    pickerInput(inputId = "select_contrast_progeny",
                label = "Select Sample/Contrast",
                choices = choices,
                selected = choices[1])
  }
})

# select pathway
output$select_pathway = renderUI({
  if (!is.null(input$select_contrast_progeny)) {
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
    
  }
})

# Bar plot with the TFs for a condition------------------------------------

barplot_nes_reactive_progeny = reactive ({
  if (!is.null(input$select_contrast_progeny)) {
    p <- P() %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column(var = "pathways") %>%
      barplot_nes_progeny(smpl = input$select_contrast_progeny)
  }
  
})

scatter_reactive = reactive({
  if (!is.null(input$select_contrast_progeny) &
      !is.null(input$select_pathway)) {
    
    if (input$example_data){
      organism = "Human"
    }else {organism = input$organism}

    prog_matrix <- progeny::getModel(organism = organism, top =  input$top) %>%
      tibble::rownames_to_column("GeneID") %>%
      dplyr::select(GeneID, input$select_pathway)
    
    inputProgeny_df <- expr() %>%
      as.data.frame() %>%
      dplyr::select(input$select_contrast_progeny) %>%
      tibble::rownames_to_column("GeneID")
    
    title = paste0(
      "weights of ",    
      input$select_pathway,
      " for sample/contrast ",
      input$select_contrast_progeny
    )
    
    scat_plots <- scater_pathway(df = inputProgeny_df,
                                 weight_matrix = prog_matrix,
                                 title = title)
  }
  
})

# Render Tables -----------------------------------------------------------
# TF-activities
output$progeny_table = DT::renderDataTable({
  # DT::datatable(P())
  results_progeny = P() %>%
    t() %>%
    as.data.frame() %>%
    round(digits = 3) %>%
    rownames_to_column(var = "Pathways")
  
  result_matrix = DT::datatable(
    results_progeny,
    option = list(
      scrollX = TRUE,
      autoWidth = T,
      pageLength = 14
    ),
    filter = "top"
  )
})

# Render Plots ------------------------------------------------------------

# Bar plot with the TFs for a condition
output$barplot_progeny = renderPlot({
  print(barplot_nes_reactive_progeny())
})

# Bar plot of activity for all conditions for a TF
output$scatter = renderPlot({
  plot(scatter_reactive())
})

# Heatmap for all samples and pathways
output$heatmap_scores = renderPlot({
  heatmap_scores(df = P())
})

# Download Handler --------------------------------------------------------

# All in a tar
output$download_progeny_analysis = downloadHandler(
  filename = "footprint_progeny_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_progeny_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    fnames = c(
      paste0("barplot_progeny_", input$select_contrast_progeny, ".png"),
      paste0(
        "scatter_density_progeny_",
        input$select_contrast,
        "_",
        input$select_pathway,
        ".png"
      ),
      "heatmap_progeny.png"
    )
    
    ggsave(file.path(fdir, fnames[1]), barplot_nes_reactive_progeny(), device = "png")
    ggsave(file.path(fdir, fnames[2]), scatter_reactive(), device = "png")
    ggsave(file.path(fdir, fnames[3]),
           heatmap_scores(df = P()),
           device = "png")
    write.csv(P(),
              file.path(fdir, "pathways_progeny_scores.csv"),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)

output$download_scatter = downloadHandler(
  filename = paste0(
    "scatter_density_progeny_",
    input$select_contrast_progeny,
    "_",
    input$select_pathway,
    ".png"
  ),
  content = function(x) {
    ggsave(x, scatter_reactive(), device = "png")
    
  }
)

output$download_barplot = downloadHandler(
  filename = paste0("barplot_progeny_", input$select_contrast_progeny, ".png"),
  
  content = function(x) {
    ggsave(x, barplot_nes_reactive(), device = "png")
    
  }
)

output$download_heatmap = downloadHandler(
  filename = "heatmap_progeny.png",
  content = function(x) {
    ggsave(x,  heatmap_scores(df = P()), device = "png")
    
  }
)
