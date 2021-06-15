

#initialize

library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(dorothea)
library(ggplot2)
library(reshape2)
library(tidygraph)
library(ggraph)
library(plotly)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

# PLOTS -------------------------------------------------------------

barplot_nes = function(df, smpl, nHits) {
  df = df[, c("GeneID", smpl)] %>%
    dplyr::rename(NES = smpl) %>%
    dplyr::top_n(nHits, wt = abs(NES)) %>%
    dplyr::arrange(NES) %>%
    dplyr::mutate(GeneID = factor(GeneID))
  
  title = paste("Sample/Contrast:", smpl, sep = " ")
  
  ggplot(df, aes(x = NES, y = reorder(GeneID, NES))) +
    geom_bar(aes(fill = NES), stat = "identity") +
    scale_fill_gradient2(
      low = "#99004C",
      high = "#0859A2",
      #"darkblue", "indianred"
      mid = "whitesmoke",
      midpoint = 0
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(
        hjust = 1,
        size = 15,
        face = "bold"
      ),
      axis.text.y = element_text(size = 15, face = "bold")
    ) +
    ylab("Transcription Factors") +
    xlab("Normalized Enrichment scores (NES)") +
    ggtitle(title)
}

barplot_tf = function(df, selTF) {
  df %>%
    rownames_to_column(var = "tf") %>%
    dplyr::filter(tf == selTF)  %>%
    reshape2::melt() %>%
    arrange(value) %>%
    dplyr::mutate(variable = factor(variable, variable),
                  effect = factor(sign(value), c(-1, 1))) %>%
    ggplot(aes(x = variable, y = value, fill = effect)) +
    geom_col() +
    coord_flip() +
    labs(x = "Sample/Contrast", y = "Normalized Enrichment scores (NES)") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(
        hjust = 1,
        size = 15,
        face = "bold"
      ),
      axis.text.y = element_text(size = 15, face = "bold")
    ) +
    scale_fill_manual(values = c("#99004C", "#0859A2"),
                      drop = F) +
    theme(aspect.ratio = c(1)) +
    ggtitle(paste0("TF: ", selTF))
  
}

plot_network = function(network, nodes, title) {
  edges = network %>%
    dplyr::filter(target %in% unique(nodes$target))
  colnames(edges) = c("from", "sign", "to")
  
  labels_edge = c("-1" = "inhibition", "1" = "activation")
  
  tbl_graph(nodes = nodes, edges = edges) %>%
    ggraph(layout = "nicely") +
    geom_edge_link(arrow = arrow(), aes(edge_colour = as.factor(sign))) +
    geom_node_point(aes(color = regulation), size = 10, alpha = 0.7) +
    geom_node_text(aes(label = target), vjust = 0.4) + ##colour = "#C8D1E0"
    theme_graph() +
    scale_color_manual(
      name = "",
      values = c("downregulated" = "#99004C",
                 "upregulated" = "#0859A2"),
      drop = F
    ) +
    scale_edge_color_manual(
      name = "Regulation",
      values = c("-1" = "#99004C",
                 "1" = "#0859A2"),
      breaks = unique(edges$sign),
      labels = labels_edge[names(labels_edge) %in% unique(edges$sign)],
      drop = F
    ) +
    scale_shape_manual(values = c(16, 15)) +
    theme(aspect.ratio = c(1),
          plot.title = element_text(size = 14, face = "plain")) +
    ggtitle(title)
}
