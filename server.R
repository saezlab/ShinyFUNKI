# SERVER
server = function(input, output, session) {
  
  # Dynamic widgets / RenderUI ----------------------------------------------
  
  # select contrast/sample
  output$select_contrast = renderUI({
    if ( ! is.null( progeny_result ) ) {
      choices = colnames(progeny_result) %>%
        str_sort(numeric=T)
      pickerInput(inputId = "select_contrast", 
                  label = "Select Sample/Contrast", choices = choices)
    }
  })
  
  # select TFs
  output$select_pathway = renderUI({
    if ( ! is.null(input$select_contrast) ){
      choices = unique(rownames(progeny_result))
      
      default_selected = rownames(progeny_result)[ 
        which( progeny_result[, input$select_contrast] ==
                 max(progeny_result[, input$select_contrast]) ) ]
      
      pickerInput(inputId = "select_pathway", 
                  label = "Select Pathway", 
                  choices = choices,
                  options = list("live-search" = TRUE), 
                  selected = default_selected )
      
    }
  })
  
  # Reactive Computations ---------------------------------------------------
  
  # Bar plot with the TFs for a condition
  barplot_nes_reactive = reactive ({
    
    if( ! is.null( input$select_contrast ) ) {

      p <- progeny_result %>%
        as.data.frame() %>%
        rownames_to_column(var = "pathways") %>%
        barplot_nes(smpl = input$select_contrast)
    }

  })
  
  scatter_reactive = reactive({
    
    if(! is.null( input$select_contrast ) & 
       ! is.null( input$select_pathway ) ) {
      
      prog_matrix <- progeny::getModel(organism, top) %>% 
        as.data.frame()  %>%
        tibble::rownames_to_column("GeneID") %>%
        dplyr::select(GeneID, input$select_pathway)
      
      inputProgeny_df <- inputProgeny %>% 
        as.data.frame() %>% 
        dplyr::select( input$select_contrast ) %>%
        tibble::rownames_to_column("GeneID")
      
      title = paste0("weights of ", input$select_pathway, "for sample/contrast ", input$select_contrast)
      
      scat_plots <- scater_pathway(df = inputProgeny_df, 
                                   weight_matrix = prog_matrix,
                                   tittle = title)
      
      
      
      
    }
    
  })
  
  # Render Tables -----------------------------------------------------------
  # TF-activities
  output$progeny_result = DT::renderDataTable({
    results_progeny = progeny_result %>% round(digits = 3) %>% rownames_to_column( var = "Pathways") 
    result_matrix = DT::datatable(results_progeny, 
                  option = list(scrollX = TRUE, autoWidth=T, pageLength = 14), filter = "top")
  })
  
  # Render Plots ------------------------------------------------------------
  
  # Bar plot with the TFs for a condition
  output$barplot_nes = renderPlot({
    print(barplot_nes_reactive())
  })

  # Bar plot of activity for all conditions for a TF
  output$scatter = renderPlot({
    plot(scatter_reactive())
  })
  
  # Heatmap for all samples and pathways
  output$heatmap_scores = renderPlot({
    heatmap_scores(df = progeny_result)
  })

  # Download Handler --------------------------------------------------------

  # All in a tar
  output$download_progeny_analysis = downloadHandler(

    filename = "footprint_progeny_saezLab.tar.gz",
    content = function(x) {
      
      fdir = "footprint_progeny_saezLab"
      
      if (dir.exists(fdir)){
        do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
      }else{
        dir.create(fdir)
      }
      
      fnames = c(paste0("barplot_progeny_", input$select_contrast, ".png"), 
                 paste0("scatter_density_progeny_", input$select_contrast, "_", input$select_pathway, ".png"),
                 "heatmap_progeny.png")
      
      ggsave(file.path(fdir, fnames[1]), barplot_nes_reactive(), device = "png")
      ggsave(file.path(fdir, fnames[2]), scatter_reactive(), device = "png")
      ggsave(file.path(fdir, fnames[3]), heatmap_scores(df = progeny_result), device = "png")
      write.csv(progeny_result, file.path(fdir, "pathways_progeny_scores.csv"), quote = F)
      tar(x, files = fdir, compression = "gzip")
    })
  
  output$download_scatter = downloadHandler(
    
    filename = paste0("scatter_density_progeny_", input$select_contrast, "_", input$select_pathway, ".png"),
    content = function(x) {
      
      ggsave(x, scatter_reactive(), device = "png")
      
     })
 
  output$download_barplot = downloadHandler(
    filename = paste0("barplot_progeny_", input$select_contrast, ".png"), 
    
    content = function(x) {
      ggsave(x, barplot_nes_reactive(), device = "png")
      
    })
  
  output$download_heatmap = downloadHandler(
    
    filename = "heatmap_progeny.png",
    content = function(x) {
      
      ggsave(x,  heatmap_scores(df = progeny_result), device = "png")
      
    })
  
  
}


