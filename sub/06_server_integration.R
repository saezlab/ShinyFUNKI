cor_pw_tf = eventReactive(input$run_cor_pw_tf, {
  D_tmp = read_csv("data/dorothea_scores.csv")
  P_tmp = read_csv("data/progeny_scores.csv")
  d = D_tmp %>%
    rename(feature = tf) %>%
    mutate(class = "tf") %>%
    select(-confidence)
  
  p = P_tmp %>%
    rename(feature = pathway) %>%
    mutate(class = "pathway")
  
  df = bind_rows(d,p)
  
  print(df)
  
  withProgress(message="Calculate pair-wise correlations", value=1, {
    print("run_correlation")
    correlation = crossing(pathway = p$feature, tf = d$feature) %>%
      mutate(stat = pmap(., .f=function(pathway, tf, ...) {
        df %>%
          filter(feature %in% c(pathway, tf)) %>%
          select(-feature) %>%
          spread(class, activity) %>%
          lm(tf ~ pathway, data = .) %>%
          tidy() %>%
          filter(term != "(Intercept)") %>%
          select(t = statistic) %>%
          pull()
      })) %>%
      unnest()
    print(correlation)
  })
})

output$network_pw_tf = renderPlot({
  if (!is.null(cor_pw_tf())) {
    x = cor_pw_tf() %>%
      mutate(sign = sign(stat)) %>%
      arrange(pathway, sign, stat) %>%
      group_by(pathway, sign) %>%
      top_n(1, stat * sign) %>%
      ungroup()
    
    nodes_df =x %>%
      distinct(pathway, tf) %>%
      gather(class, name) %>%
      distinct(name, class) %>%
      mutate(id = row_number()) %>%
      mutate(class = as_factor(class))
    
    edges_df = x %>%
      select(pathway, tf, stat, sign) %>%
      inner_join(rename(nodes_df, pathway=name, from=id), by="pathway") %>%
      select(-class) %>%
      inner_join(rename(nodes_df, tf=name, to=id), by="tf") %>%
      select(from, to, stat, sign) %>%
      mutate(sign = as.factor(sign))
    
    
    tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_edge_link(aes(edge_colour=sign)) + 
      geom_node_point(aes(color=class, size=class)) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("green50", "bordeaux50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("bordeaux", "green")), drop=F) +
      scale_size_manual(values = c(15,10)) +
      theme(legend.position = "none",
            aspect.ratio = c(1))
  }
})

output$cor_result = DT::renderDataTable({
  DT::datatable(cor_pw_tf(), 
                option = list(scrollX = TRUE, autoWidth=T), filter = "top") %>%
    formatSignif(which(map_lgl(cor_pw_tf(), is.numeric)))
})