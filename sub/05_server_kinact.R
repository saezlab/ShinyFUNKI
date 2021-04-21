K = eventReactive(input$run_kinact, {
      kinact_input = ppomics() %>%
        ungroup() %>%
        select(protein, contrast, logFC)
  
      withProgress(message="Calculate KinAct matrix", value=1, {
        kinact_result = run_viper(kinact_input, kinact_interactome(), 
                                  id_name = "contrast", value_name = "logFC", 
                                  gene_name = "protein", 
                                  regulator_name = "kinase")
        print(kinact_result)
        #df = run_viper(dorothea_input, regulon2, id_name = "contrast", value_name = "t")
        })
})


extended_ppomics = reactive({
  if (!is.null(kinact_selected_padj_cutoff())) {
    res = ppomics() %>%
      mutate(effect = case_when(adj.p.value <= kinact_selected_padj_cutoff() & logFC > 0 ~ "upregulated",
                                adj.p.value <= kinact_selected_padj_cutoff() & logFC < 0 ~ "downregulated",
                                TRUE ~ "not regulated"),
             importance = abs(logFC * -log10(adj.p.value)))
    print(res)
    return(res)
  }
})



kinact_selected_top_n_hits = eventReactive(input$kinact_selected_top_n_hits, {
  input$kinact_selected_top_n_hits
})

kinact_selected_top_n_labels = eventReactive(input$kinact_selected_top_n_labels, {
  input$kinact_selected_top_n_labels
})

kinact_selected_contrast = eventReactive(input$kinact_selected_contrast, {
  input$kinact_selected_contrast
})

selected_kinase = eventReactive(input$selected_kinase, {
  input$selected_kinase
})

kinact_selected_padj_cutoff = eventReactive(input$kinact_padj_cutoff, {
  input$kinact_padj_cutoff
})
# 
kinact_network_df = reactive({
  if (!is.null(selected_kinase()) & !is.null(kinact_selected_contrast())) {
    proteins_of_interest = extended_ppomics() %>%
      ungroup() %>%
      filter(contrast == kinact_selected_contrast()) %>%
      select(target=protein, importance, effect)
    
    kinase_of_interest = K() %>% 
      filter(contrast == kinact_selected_contrast()) %>%
      filter(tf %in% selected_kinase()) %>%
      mutate(regulation = case_when(activity >= 0 ~ "upregulated",
                                    activity < 0 ~ "downregulated")) %>%
      select(-activity)

    network = kinact_interactome() %>%
      mutate(mor = as.factor(mor)) %>%
      filter(kinase %in% selected_kinase()) %>%
      left_join(proteins_of_interest, by = "target") %>%
      inner_join(kinase_of_interest, by="kinase")
  }
})

# Plot lollipop
output$kinact_lollipop = renderPlot({
  
  K() %>% filter(contrast %in% kinact_selected_contrast()) %>%
    plot_lollipop(top_n_hits = kinact_selected_top_n_hits(), var = kinase, var_label = "Kinase")
})

# Plot heatmap
output$kinact_heatmap = renderPlot({
  K() %>%
    plot_heatmap(var="kinase")
})
# 
# select TFs
output$select_kinase = renderUI({
  pickerInput(inputId = "selected_kinase",
              label = "Select Kinase", choices = unique(filter(K(), contrast %in% kinact_selected_contrast())$kinase), options = list("live-search" = TRUE), selected = arrange(filter(K(), contrast %in% kinact_selected_contrast()), -activity)$kinase[1])
})
# 
# select contrast
output$kinact_select_contrast = renderUI({
  pickerInput(inputId = "kinact_selected_contrast",
              label = "Select Contrast", unique(ppomics()$contrast))
})
# 
# select top n results
output$kinact_select_top_n_hits = renderUI({
  if (!is.null(K())) {
    max_kinases = K() %>%
      distinct(kinase) %>%
      nrow()
    sliderInput("kinact_selected_top_n_hits",
                label = "# displayed Kinases (per category)", value = 10,
                min = 1, max=max_kinases, step=1)
  }
  })
# 
# select top n labels
output$kinact_select_top_n_labels = renderUI({
  if (!is.null(selected_kinase()) & !is.null(kinact_selected_contrast())) {
    e = extended_ppomics() %>%
      filter(contrast %in% kinact_selected_contrast() & effect != "not regulated") %>%
      rename(target=protein)

    max_labels = kinact_interactome() %>%
      filter(kinase == selected_kinase()) %>%
      inner_join(e, by="target") %>%
      nrow()

    sliderInput("kinact_selected_top_n_labels",
                label = "# shown labels in Volcano plot/nework", value = 10,
                min = 0, max=max_labels, step=1)
  }
})
# 
# 
# volcano plot
output$kinase_volcano = renderPlot({
  if (!is.null(selected_kinase()) & !is.null(kinact_selected_contrast()) & !is.null(kinact_selected_top_n_labels())) {
    renamed_extended_ppomics = extended_ppomics() %>%
      filter(contrast == kinact_selected_contrast()) %>%
      rename(target = protein)
      
    sub_kinact_interactome = kinact_interactome() %>%
      filter(kinase == selected_kinase())
    
    plot_volcano(renamed_extended_ppomics, sub_kinact_interactome, selected_top_n_labels = kinact_selected_top_n_labels(), var=kinase, var_label = "Kinase")
  }
})
# 
# # tf plot
# output$tf_bar = renderPlot({
#   if (!is.null(selected_tf())) {
#     D() %>%
#       filter(tf == selected_tf()) %>%
#       arrange(activity) %>%
#       mutate(contrast = factor(contrast, contrast),
#              effect = factor(sign(activity), c(-1,1))) %>%
#       ggplot(aes(x=contrast, y=activity, fill=effect)) +
#       geom_col() +
#       coord_flip() +
#       labs(x="Contrast", y = "Activity") +
#       theme_minimal() +
#       theme(legend.position = "none") +
#       scale_fill_manual(values = rwth_color(c("magenta", "green")),
#                         drop=F) +
#       theme(aspect.ratio = c(1))
#   }
# })
# 
output$kinact_network = renderPlot({
  if (!is.null(kinact_network_df()) & !is.null(kinact_selected_top_n_labels())) {
    plot_network(network = kinact_network_df(),
                    num_nodes = kinact_selected_top_n_labels(),
                    var="kinase", var_label = "Kinase")
  }
})
# 
# show Kinase activity df
output$kinact_result = DT::renderDataTable({
  kinact_result_matrix = K() %>%
    spread(contrast, activity)
  DT::datatable(kinact_result_matrix, option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
    formatSignif(which(map_lgl(kinact_result_matrix, is.numeric)))
})
# 
# # download tf activities
# output$download_dorothea_scores = downloadHandler(
#   filename = "dorothea_scores.csv",
#   content = function(x) {
#     write_csv(D(), x)
#   })
# 
# # download network
# output$download_displayed_network = downloadHandler(
#   filename = function() {
#     paste0("network_", selected_tf(), ".sif")
#   },
#   content = function(file) {
#     if (!is.null(dorothea_network_df())) {
#       dorothea_network_df() %>%
#         select(tf, mor, target) %>%
#         write_delim(., file, col_names = F)
#     }
# 
#   })