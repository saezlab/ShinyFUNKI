library(shiny)
library(shinyWidgets)
library(tidyverse)
library(viper)
library(myutils)
library(ggrepel)
library(DT)
library(furrr)
library(pheatmap)
library(shinyjs)
library(ggExtra)
library(tidygraph)
library(ggraph)
library(broom)



plot_lollipop = function(df, top_n_hits, var, var_label) {
  var = enquo(var)
  title = paste("Contrast:", unique(df$contrast))
  df %>% 
    arrange(activity) %>%
    mutate(!!var := as_factor(!!var),
           effect = factor(sign(activity)),
           abs_activity = abs(activity)) %>%
    group_by(effect) %>%
    top_n(top_n_hits, abs_activity) %>%
    ungroup() %>%
    ggplot(aes(x=!!var, y=activity, color=effect)) +
    geom_segment(aes(x=!!var, xend=!!var, y=0, yend=activity), color="grey") +
    geom_point(size=4) +
    coord_flip() +
    theme_light() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    labs(x = var_label, y="Activity (z-score)") +
    scale_color_manual(values = rwth_color(c("magenta", "green"))) +
    theme(legend.position = "none") +
    theme(aspect.ratio = c(1)) + 
    ggtitle(title)
}

plot_heatmap = function(df, var="tf") {
  mat = df %>%
    select(!!var, contrast, activity) %>% 
    spread(contrast, activity) %>%
    data.frame(row.names = var, check.names = F)
  
  if (ncol(mat) > 1) {
    pheatmap(mat, show_rownames = F)
  } else if (ncol(mat) == 1) {
    pheatmap(mat, cluster_rows = F, cluster_cols = F, show_rownames = F)
  }
}

plot_network = function(network, num_nodes, var="tf", var_label = "TF") {
  var_of_interest = unique(network[[var]])
  
  feature_name = network %>% pull(!!var) %>% unique()
  contrast_name = network %>% pull(contrast) %>% unique()
  title = paste0("Contrast: ", contrast_name, ", ", var_label,": ", feature_name)
  
  sub_network = network %>% 
    drop_na() %>%
    filter(effect != "not regulated") %>%
    arrange(-importance) %>%
    mutate(n = row_number()) %>%
    filter(n <= num_nodes) %>%
    select(!!var, target, mor, effect, regulation)
  
  if (nrow(sub_network) > 0) {
    nodes_df = sub_network %>%
      distinct_(var, "target", "effect", "regulation") %>%
      gather(class, name, -effect, -regulation) %>%
      mutate(effect = case_when(class != var ~ effect,
                                class == var ~ regulation)) %>%
      mutate(effect = factor(effect, c("upregulated", "downregulated"))) %>%
      distinct(class, name, effect) %>%
      mutate(id = row_number())
    
    edges_df = sub_network %>%
      select(!!var, target, mor) %>%
      inner_join(rename(nodes_df, !!var:=name, from=id), by=var) %>%
      select(-c(class,effect)) %>%
      inner_join(rename(nodes_df, target=name, to=id), by="target") %>%
      select(from, to, mor)
    
    g = tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_edge_link(arrow = arrow(), aes(edge_colour=mor)) + 
      geom_node_point(aes(color = effect, shape=class), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("green50", "bordeaux50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("bordeaux", "green")), drop=F) +
      scale_shape_manual(values = c(16,15)) +
      theme(legend.position = "none",
            aspect.ratio = c(1), 
            plot.title = element_text(size = 14, face="plain")) +
      ggtitle(title)
  } else {
    nodes_df = sub_network %>%
      distinct_(var, "target", "effect", "regulation") %>%
      gather(class, name, -effect, -regulation) %>%
      mutate(effect = case_when(class != var ~ effect,
                                class == var ~ regulation)) %>%
      mutate(effect = factor(effect, c("upregulated", "downregulated"))) %>%
      distinct(class, name, effect) %>%
      filter(class == "tf") %>%
      mutate(id = row_number())
    edges_df = NULL
    
    g = tbl_graph(nodes = nodes_df, edges = edges_df) %>%
      ggraph(layout = "nicely") + 
      geom_node_point(aes(color = effect, shape=class), size=10) +
      geom_node_text(aes(label = name), vjust = 0.4) + 
      theme_graph() +
      scale_color_manual(values = rwth_color(c("green50", "red50")), drop=F) +
      scale_edge_color_manual(values = rwth_color(c("red", "green")), drop=F) +
      scale_shape_manual(values = c(15)) +
      theme(legend.position = "none",
            aspect.ratio = c(1)) +
      ggtitle(title)
  }
  return(g)
}

plot_volcano = function(df, interactome, selected_top_n_labels, var, var_label = "TF") {
  var = enquo(var)
  
  feature_name = interactome %>% pull(!!var) %>% unique()
  contrast_name = df %>% pull(contrast) %>% unique()
  
  title = paste0("Contrast: ", contrast_name, ", ", var_label,": ", feature_name)
  
  interactome %>% 
    inner_join(df, by=c("target")) %>%
    mutate(effect = factor(effect, c("downregulated", "not regulated", "upregulated")),
           regulated = case_when(effect %in% c("downregulated", "upregulated") ~ "yes",
                                 effect %in% c("not regulated") ~ "no")) %>%
    arrange(desc(regulated), -importance) %>%
    mutate(n = row_number(),
           label = case_when(n <= selected_top_n_labels & effect != "not regulated" ~ target,
                             TRUE ~ "")) %>%
    ggplot(aes(x=logFC, y=-log10(adj.p.value), label=label, color=effect)) +
    geom_point() +
    geom_label_repel() +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = rwth_color(c("bordeaux", "black50","green")),
                       drop=F) +
    theme(aspect.ratio = c(1)) +
    ggtitle(title)
}