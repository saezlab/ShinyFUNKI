# SERVER
server = function(input, output, session) {
  
  # Dynamic widgets / RenderUI ----------------------------------------------
  
  # select contrast/sample
  output$select_contrast = renderUI({
    if ( ! is.null( dorothea_result ) ) {
      choices = colnames(dorothea_result) %>%
        str_sort(numeric=T)
      pickerInput(inputId = "select_contrast", 
                  label = "Select Sample/Contrast", choices = choices)
    }
  })
  
  # select TFs
  output$select_tf = renderUI({
    if ( ! is.null(input$select_contrast) ){
      choices = unique(rownames(dorothea_result))
      
      default_selected = rownames(dorothea_result)[ 
        which( dorothea_result[, input$select_contrast] ==
                 max(dorothea_result[, input$select_contrast]) ) ]
      
      pickerInput(inputId = "select_tf", 
                  label = "Select Transcription Factor", 
                  choices = choices,
                  options = list("live-search" = TRUE), 
                  selected = default_selected )
      
    }
  })
  
  # select number of targets
  output$select_top_n_labels = renderUI({
    if ( ! is.null(input$select_tf) ) {
      
      targets = dorothea_hs %>% 
        dplyr::filter(tf == input$select_tf & confidence %in% confidence_level) %>%
        dplyr::select(target) %>% 
        dplyr::filter(target %in% rownames(inputDorothea)) %>%
        nrow()
      
      sliderInput("select_top_n_labels",
                  label = "Number of targets to display", 
                  value = dplyr::case_when(targets > 10 ~ 10, targets <= 10 ~ round(targets*(2/3))), 
                  min = 1, max = targets, step = 1)
    }
  })
  
  # select top n results
  output$select_top_n_hits = renderUI({
    if ( ! is.null( dorothea_result ) ) {
      max_tfs = length( unique( rownames( dorothea_result ) ) )
      sliderInput("select_top_n_hits",
                  label = "Numer of Transcription Factors to display", 
                  value = 25, min = 1, max = max_tfs, step = 1)
    }
  })

  
  # Reactive Computations ---------------------------------------------------
  
  # Bar plot with the TFs for a condition
  barplot_nes_reactive = reactive ({
    
    if( ! is.null( input$select_contrast ) &
        ! is.null( input$select_top_n_hits )  
      ) {

      p <- dorothea_result %>%
        as.data.frame() %>%
        rownames_to_column(var = "GeneID") %>%
        barplot_nes(smpl = input$select_contrast, nHits = input$select_top_n_hits)
    }

  })
  
  barplot_tf_reactive = reactive({
    
    if(!is.null(input$select_tf)) {
      
      q <- dorothea_result %>%
        barplot_tf(selTF = input$select_tf)
      
    }
    
  })
  
  network_tf_reactive = reactive({

    if ( !is.null(input$select_tf) & 
         !is.null(input$select_contrast) ) {
      
      aux = dorothea_hs %>% 
        dplyr::filter(tf == input$select_tf & 
                        confidence %in% confidence_level)
      
      nodes = merge.data.frame(aux %>% 
                                 dplyr::select(target), 
                               inputDorothea %>% 
                                 dplyr::select(input$select_contrast) %>% 
                                 tibble::rownames_to_column("target"), 
                               by = "target")
      
      nodes = nodes[order(abs(nodes[,input$select_contrast]),decreasing = T),] 
      
      if ( input$select_top_n_labels <= nrow(nodes) ){
        nodes = nodes[1:input$select_top_n_labels,] 
      }
      
      nodes = tibble(rbind.data.frame(nodes,
                                      dorothea_result %>%
                                        tibble::rownames_to_column("target") %>%
                                        dplyr::filter(target == input$select_tf) %>%
                                        dplyr::select(target, input$select_contrast))) 
      
      nodes$regulation = nodes[, input$select_contrast] 
      
      nodes = nodes %>%
        mutate(regulation = dplyr::case_when(regulation >= 0 ~ "upregulated",
                                             regulation < 0 ~ "downregulated"))
      
      gtitle = paste0("Sample/Contrast: ", input$select_contrast, "; TF: ", input$select_tf)
      
      plot_network(network = aux %>% dplyr::select(tf, mor, target), 
                   nodes = nodes, 
                   title = gtitle)
     
    }
    
    
  })


  # Render Tables -----------------------------------------------------------
  # TF-activities
  output$dorothea_result = DT::renderDataTable({
    results_confidence = unique.data.frame(merge(dorothea_hs[, c("tf", "confidence")], 
                                                 dorothea_result %>% round(digits = 3) %>% rownames_to_column(var = "tf"), by="tf"))
    colnames(results_confidence)[ colnames(results_confidence) == "tf" ] = "Transcription Factor"
    dorothea_result_matrix = DT::datatable(results_confidence, 
                  option = list(scrollX = TRUE, autoWidth=T), filter = "top")
  })
  
  # Render Plots ------------------------------------------------------------
  
  # Bar plot with the TFs for a condition
  output$barplot_nes = renderPlot({
    print(barplot_nes_reactive())
  })

  # Bar plot of activity for all conditions for a TF
  output$tf_bar = renderPlot({
    print(barplot_tf_reactive())
  })
  
  # Network of a TF with it's targets
  output$tf_network = renderPlot({
    print(network_tf_reactive())
  })

  # Download Handler --------------------------------------------------------

  # All in a tar
  output$download_dorothea_analysis = downloadHandler(

    filename = "footprint_dorothea_saezLab.tar.gz",
    content = function(x) {
      
      fdir = "footprint_dorothea_saezLab"
      
      if (dir.exists(fdir)){
        do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
      }else{
        dir.create(fdir)
      }
      
      fnames = c(paste0("barplot_tfs_", input$select_contrast, ".png"), 
                 paste0("barplot_samples_", input$select_tf, ".png"),
                 paste0("network_", input$select_contrast, "_", input$select_tf, ".png"))
      
      ggsave(file.path(fdir, fnames[1]), barplot_nes_reactive(), device = "png")
      ggsave(file.path(fdir, fnames[2]), barplot_tf_reactive(), device = "png")
      ggsave(file.path(fdir, fnames[3]), network_tf_reactive(), device = "png")
      write.csv(dorothea_result, file.path(fdir, "TFactivities_nes.csv"), quote = F)
      tar(x, files = fdir, compression = "gzip")
    })
  
}


