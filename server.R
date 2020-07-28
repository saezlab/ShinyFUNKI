# SERVER
server = function(input, output, session) {
  
  
  # Reactive Computations ---------------------------------------------------
  
  #  dorothea_network_df = reactive({
  # })
  
  # Dynamic widgets / RenderUI ----------------------------------------------
  
  # select contrast/sample
  output$dorothea_select_contrast = renderUI({
    if ( ! is.null( dorothea_result ) ) {
      choices = colnames(dorothea_result) %>%
        str_sort(numeric=T)
      pickerInput(inputId = "dorothea_selected_contrast", 
                  label = "Select Sample/Contrast", choices = choices)
    }
  })
  
  # select TFs
  output$select_tf = renderUI({
    choices = unique(rownames(dorothea_result))
    
    default_selected = rownames(dorothea_result)[ 
                          which( dorothea_result[, input$dorothea_selected_contrast] ==
                                   max(dorothea_result[, input$dorothea_selected_contrast]) ) ]
    
    pickerInput(inputId = "selected_tf", 
                label = "Select Transcription Factor", 
                choices = choices,
                options = list("live-search" = TRUE), 
                selected = default_selected
    )
  })
  
  # select number of targets
  output$dorothea_select_top_n_labels = renderUI({
    
    if ( ! is.null(input$selected_tf) ) {
      targets = dorothea_hs %>% 
        dplyr::filter(tf == input$selected_tf & confidence %in% confidence_level) %>%
        dplyr::select(target) %>% nrow()
      
      sliderInput("dorothea_selected_top_n_labels",
                  label = "Number of targets to display", value = 10,
                  min = 1, max = targets, step = 1)
    }
  })
  
  # select top n results
  output$select_top_n_hits = renderUI({
    if ( ! is.null( dorothea_result ) ) {
      max_tfs = length( unique( rownames( dorothea_result ) ) )
      sliderInput("selected_top_n_hits",
                  label = "Numer of Transcription Factors to display", value = 25,
                  min = 1, max=max_tfs, step = 1)
    }
  })
  

  # Render Tables -----------------------------------------------------------
  # TF-activities
  output$dorothea_result = DT::renderDataTable({
    results_confidence = unique.data.frame(merge(dorothea_hs[, c("tf", "confidence")], dorothea_result %>% rownames_to_column(var = "tf"), by="tf"))
    colnames(results_confidence)[ colnames(results_confidence) == "tf" ] = "Transcription Factor"
    dorothea_result_matrix = DT::datatable(results_confidence, 
                  option = list(scrollX = TRUE, autoWidth=T), filter = "top")
  })
  
  # Render Plots ------------------------------------------------------------
  
  # Bar plot with the TFs for a condition
  output$barplot_nes = renderPlot({
    
    dorothea_result %>% 
      as.data.frame() %>%
      rownames_to_column(var = "GeneID") %>%
      barplot_nes(smpl = input$dorothea_selected_contrast, nHits = input$selected_top_n_hits)
    
  })

  # Bar plot of activity for all conditions for a TF
  output$tf_bar = renderPlot({
    if(!is.null(input$selected_tf)) {
      dorothea_result %>%
        barplot_tf(selTF = input$selected_tf)
    }
    
  })
  
  # Network of a TF with it's targets
  output$tf_network = renderPlot({
    print(paste0("Nlabels: ", input$dorothea_select_top_n_labels))
    print(paste0("contrast: ",input$dorothea_selected_contrast))
    print(paste0("TF: ",input$selected_tf))
    print(paste0("top TF: ", input$select_top_n_hits))
    print(lapply(input$dorothea_network_df, head))
    
      if ( !is.null(input$selected_tf) & 
           !is.null(input$dorothea_selected_contrast) ) {
        
        aux = dorothea_hs %>% 
          dplyr::filter(tf == input$selected_tf & 
                          confidence %in% confidence_level)
        
        nodes = merge.data.frame(aux %>% 
                                   dplyr::select(target), 
                                 inputDorothea %>% 
                                   dplyr::select(input$dorothea_selected_contrast) %>% 
                                   tibble::rownames_to_column("target"), 
                                 by = "target")
        
        nodes = nodes[order(abs(nodes[,input$dorothea_selected_contrast]),decreasing = T),] 
        
        #nodes = nodes[1:input$dorothea_select_top_n_labels,]
        
        nodes = tibble(rbind.data.frame(nodes,
                                 dorothea_result %>%
                                   tibble::rownames_to_column("target") %>%
                                   dplyr::filter(target == input$selected_tf) %>%
                                   dplyr::select(target, input$dorothea_selected_contrast))) 
        
        nodes$regulation = nodes[, input$dorothea_selected_contrast] 
         
        nodes = nodes %>%
                mutate(regulation = dplyr::case_when(regulation >= 0 ~ "upregulated",
                                                     regulation < 0 ~ "downregulated"))

        gtitle = paste0("Sample/Contrast: ", input$dorothea_selected_contrast, "; TF: ", input$selected_tf)
        
        plot_network(network = aux %>% dplyr::select(tf, mor, target), 
                     nodes = nodes, 
                     title = gtitle)
        
        }
      
  })



  # Download Handler --------------------------------------------------------
  # TF-activities
  output$download_dorothea_scores = downloadHandler(
    filename = "TFactivities_nes.csv",
    content = function(x) {
      write.csv(dorothea_result, x, quote=F)
      #tar('tmp.tar.gz', 'tmp.txt', compression = 'gzip', tar="tar")
    })
  
}
