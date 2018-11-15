# Reactive Computations ---------------------------------------------------
D = eventReactive({
  input$run_dorothea
  input$selected_conf_level
  input$quality_vs_coverage
}, {
  
  if (!is.null(selected_conf_level()) & num_run_dorothea() > 0) {
    print(num_run_dorothea())
    dorothea_input = expr() %>%
      ungroup() %>%
      select(gene, contrast, logFC)
    
    withProgress(message="Calculate TF activities...", value=1, {
      filtered_interactome = interactome() %>% 
        filter(confidence %in% selected_conf_level())
      
      dorothea_result = run_viper(dorothea_input, filtered_interactome, 
                                  id_name = "contrast", value_name = "logFC", 
                                  regulator_name = "tf")
      switch(quality_vs_coverage(),
             "Quality" = dorothea_result,
             "Coverage" = distinct(dorothea_result, tf, contrast, activity))
    })
  }
})



dorothea_network_df = reactive({
  if (!is.null(selected_tf()) & !is.null(dorothea_selected_contrast()) & !is.null(D())) {
    genes_of_interest = extended_expr() %>%
      ungroup() %>%
      filter(contrast == dorothea_selected_contrast()) %>%
      select(target=gene, importance, effect)
    
    tf_of_interest = D() %>% 
      filter(contrast == dorothea_selected_contrast()) %>%
      filter(tf %in% selected_tf()) %>%
      mutate(regulation = case_when(activity >= 0 ~ "upregulated",
                                    activity < 0 ~ "downregulated")) %>%
      select(-activity)
    
    network = interactome() %>%
      filter(confidence %in% selected_conf_level()) %>%
      mutate(mor = as.factor(mor)) %>%
      filter(tf %in% selected_tf()) %>%
      left_join(genes_of_interest, by = "target") %>%
      inner_join(tf_of_interest, by=c("tf"))
  }
})



# Reactive widgets input --------------------------------------------------
quality_vs_coverage = eventReactive(input$quality_vs_coverage, {
  input$quality_vs_coverage
})

num_run_dorothea = eventReactive(input$run_dorothea, {
  input$run_dorothea
})

selected_top_n_hits = eventReactive(input$selected_top_n_hits, {
  input$selected_top_n_hits
})

selected_conf_level = eventReactive(input$selected_conf_level, {
  input$selected_conf_level
})

dorothea_selected_top_n_labels = eventReactive(input$dorothea_selected_top_n_labels, {
  input$dorothea_selected_top_n_labels
})

dorothea_selected_contrast = eventReactive(input$dorothea_selected_contrast, {
  input$dorothea_selected_contrast
})

selected_tf = eventReactive(input$selected_tf, {
  input$selected_tf
})

dorothea_selected_padj_cutoff = eventReactive(input$dorothea_padj_cutoff, {
  input$dorothea_padj_cutoff
})



# Dynamic widgets / RenderUI ----------------------------------------------
# select TFs
output$select_tf = renderUI({
  choices = D() %>%
    filter(contrast %in% dorothea_selected_contrast()) %>%
    distinct(tf) %>% 
    pull()
  
  default_selected = D() %>%
    filter(contrast == dorothea_selected_contrast()) %>%
    arrange(-activity) %>%
    slice(1) %>%
    pull(tf)
  
  pickerInput(inputId = "selected_tf", 
              label = "Select TF", 
              choices = choices,
              options = list("live-search" = TRUE), 
              selected = default_selected
              )
})

# select contrast
output$dorothea_select_contrast = renderUI({
  if (!is.null(expr())) {
    choices = expr() %>% 
      distinct(contrast) %>%
      pull() %>%
      str_sort(numeric=T)
    pickerInput(inputId = "dorothea_selected_contrast", 
                label = "Select Contrast", choices = choices)
  }
})

# select top n results
output$select_top_n_hits = renderUI({
  if (!is.null(D())) {
    max_tfs = D() %>%
      distinct(tf) %>%
      nrow()
    sliderInput("selected_top_n_hits",
                label = "# displayed TFs (per category)", value = 10,
                min = 1, max=max_tfs, step=1)
  }
})

# select top n labels
output$dorothea_select_top_n_labels = renderUI({
  if (!is.null(selected_tf()) & !is.null(dorothea_selected_contrast())) {
    e = extended_expr() %>%
      filter(contrast %in% dorothea_selected_contrast() & 
               effect != "not regulated") %>%
      rename(target=gene)
    
    max_labels = interactome() %>% 
      filter(confidence %in% selected_conf_level()) %>%
      filter(tf == selected_tf()) %>% 
      inner_join(e, by="target") %>%
      nrow()
    
    sliderInput("dorothea_selected_top_n_labels",
                label = "# shown labels in Volcano plot/nework", value = 10,
                min = 0, max=max_labels, step=1)
  }
})




# Render Plots ------------------------------------------------------------
# Lollipop
output$dorothea_lollipop = renderPlot({
  D() %>% filter(contrast %in% dorothea_selected_contrast()) %>%
    plot_lollipop(top_n_hits = selected_top_n_hits(), 
                  var = tf, var_label = "Transcription Factor")
})

# Heatmap
output$dorothea_heatmap = renderPlot({
  D() %>% 
    plot_heatmap(var="tf")
})

# Volcano
output$tf_volcano = renderPlot({
  if (!is.null(selected_tf()) & 
      !is.null(dorothea_selected_contrast()) & 
      !is.null(dorothea_selected_top_n_labels())) {
    renamed_extended_expr = extended_expr() %>%
      filter(contrast == dorothea_selected_contrast()) %>%
      rename(target = gene)
    
    filtered_interactome  = interactome() %>%
      filter(confidence %in% selected_conf_level()) %>%
      filter(tf == selected_tf())
    
    plot_volcano(renamed_extended_expr, filtered_interactome, 
                 selected_top_n_labels = dorothea_selected_top_n_labels(), 
                 var=tf, var_label = "TF")
  }
})

# Bar
output$tf_bar = renderPlot({
  if (!is.null(selected_tf())) {
    D() %>%
      filter(tf == selected_tf()) %>%
      arrange(activity) %>%
      mutate(contrast = factor(contrast, contrast),
             effect = factor(sign(activity), c(-1,1))) %>%
      ggplot(aes(x=contrast, y=activity, fill=effect)) +
      geom_col() +
      coord_flip() +
      labs(x="Contrast", y = "Activity") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = rwth_color(c("magenta", "green")),
                        drop=F) +
      theme(aspect.ratio = c(1)) +
      ggtitle(paste0("TF: ", selected_tf()))
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


# Render Tables -----------------------------------------------------------
# TF-activities
output$dorothea_result = DT::renderDataTable({
  dorothea_result_matrix = D() %>%
    spread(contrast, activity) %>%
    rename(TF = tf)
  DT::datatable(dorothea_result_matrix, 
                option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
    formatSignif(which(map_lgl(dorothea_result_matrix, is.numeric)))
})

# Download Handler --------------------------------------------------------
# TF-activities
output$download_dorothea_scores = downloadHandler(
  filename = "tf_activities.csv",
  content = function(x) {
    write_csv(D(), x)
  })

# Network
output$download_network = downloadHandler(
  filename = function() {
    paste0("network_", selected_tf(), dorothea_selected_contrast(), ".sif")
  },
  content = function(file) {
    if (!is.null(dorothea_network_df())) {
      dorothea_network_df() %>%
        select(tf, mor, target) %>%
        write_delim(., file, col_names = F)
    }
    
  })