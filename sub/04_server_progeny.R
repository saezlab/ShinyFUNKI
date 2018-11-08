P = eventReactive(input$run_progeny, {
  progeny_input = expr() %>%
    ungroup() %>%
    select(gene, contrast, t = statistic)
  
  withProgress(message="Calculate PROGENy matrix", value=1, {
    print("run_progeny")
    progeny_result = run_progeny(progeny_input, matrix(), id_name = "contrast", value_name = "t", permutation = 10)
    print(progeny_result)
  })
})



progeny_selected_top_n_labels = eventReactive(input$progeny_selected_top_n_labels, {
  input$progeny_selected_top_n_labels
})

progeny_selected_contrast = eventReactive(input$progeny_selected_contrast, {
  input$progeny_selected_contrast
})

# Plot lollipop
output$progeny_lollipop = renderPlot({
  #if (!is.null(input$file)){
  P() %>%
    filter(contrast %in% progeny_selected_contrast()) %>%
    plot_lollipop(top_n_hits = 100, var = pathway, var_label = "Pathway")
  #}
})

# select TFs
output$select_pathway = renderUI({
  pickerInput(inputId = "selected_pathway",
              label = "Select pathway", choices = unique(filter(P(), contrast %in% progeny_selected_contrast())$pathway), options = list("live-search" = TRUE), selected = arrange(filter(P(), contrast %in% progeny_selected_contrast()), -activity)$pathway[1])
})

# select contrast
output$progeny_select_contrast = renderUI({
  pickerInput(inputId = "progeny_selected_contrast",
              label = "Select Contrast", unique(expr()$contrast))
})

# volcano plot
scatter_plot = eventReactive({
  input$selected_pathway
  input$progeny_selected_top_n_labels
}, {
  if (!is.null(input$selected_pathway)) {
    scatter = matrix() %>% filter(pathway == input$selected_pathway) %>%
      inner_join(expr(), by=c("gene")) %>%
      filter(contrast %in% progeny_selected_contrast()) %>%
      mutate(contribution = weight * statistic,
             abs_contribution = abs(contribution)) %>%
      arrange(pathway, -abs_contribution) %>%
      group_by(pathway) %>%
      mutate(importance = 1:n()) %>%
      ungroup() %>%
      mutate(lab = NA) %>%
      mutate(lab = case_when(importance <= input$progeny_selected_top_n_labels ~ gene),
             effect = case_when(sign(contribution) == 1 ~ "positive",
                                sign(contribution) == -1 ~ "negative"),
             effect = factor(effect, c("negative", "positive")),
             alpha_param = case_when(!is.na(lab) ~ "1",
                                     TRUE ~ "0")
      ) %>%
      ggplot(aes(x=statistic, y=weight, label=lab, color=effect)) +
      geom_point(aes(alpha = alpha_param)) +
      geom_hline(yintercept = c(0),
                 linetype=c(1) ,
                 color=c("black")) +
      geom_vline(xintercept = c(0),
                 linetype=c(1) , color=c("black")) +
      geom_label_repel(show.legend = F, na.rm = T) +
      theme_minimal() +
      theme(aspect.ratio = c(1)) +
      scale_alpha_manual(values = c(0.25,1), guide="none") +
      scale_color_manual(values = rwth_color(c("magenta", "green")),
                         drop=F) +
      labs(x="Effect size", y="PROGENy weight")
    
    ggMarginal(scatter, type="histogram")
  }

})

output$pathway_scatter = renderPlot({
  scatter_plot()
})

# show TF activity df
output$progeny_result = DT::renderDataTable({
  progeny_result_matrix = P() %>%
    spread(contrast, activity) %>%
    rename(Pathway = pathway)
  DT::datatable(progeny_result_matrix, option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
    formatSignif(which(map_lgl(progeny_result_matrix, is.numeric)))
})

# download tf activities
output$download_progeny_scores = downloadHandler(
  filename = "progeny_scores.csv",
  content = function(x){
    write_csv(P(), x)
  })