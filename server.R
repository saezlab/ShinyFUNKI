# SERVER
server = function(input, output, session) {
  
  # Dynamic widgets / RenderUI ----------------------------------------------
  
  # select contrast
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
                          which( dorothea_result[, input$dorothea_selected_contrast] == max(dorothea_result[, input$dorothea_selected_contrast]) ) ]
    
    pickerInput(inputId = "selected_tf", 
                label = "Select Transcription Factor", 
                choices = choices,
                options = list("live-search" = TRUE), 
                selected = default_selected
    )
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
  
  # Network
  output$tf_network = renderPlot({
    if (!is.null(dorothea_network_df()) & 
        !is.null(dorothea_selected_top_n_labels())) {
      plot_network(network = dorothea_network_df(), 
                   num_nodes = dorothea_selected_top_n_labels(), var="tf", var_label = "TF")
    }
  })
  
  
  
  
  # Lollipop
  # output$dorothea_lollipop = renderPlot({
  #   dorothea_result[, dorothea_selected_contrast()]  %>%
  #     plot_lollipop(top_n_hits = selected_top_n_hits(), 
  #                   var = tf, var_label = "Transcription Factor")
  # })
  
}