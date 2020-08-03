
#initialize

library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(reshape2)
library(tidygraph)
library(ggraph)
library(plotly)
library(pheatmap)
library(progeny)
library(gridExtra)
library(cowplot)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# Load data

# input for dorothea
inputProgeny <- read.csv("data/examples/progeny_example.csv", row.names = 1 )

# dorothea results
p_file = "data/examples/progeny_scores_Human_100.csv"
progeny_result = read.csv( p_file, row.names = 1 ) # from VRE
rownames(progeny_result) = gsub(".", "-", rownames(progeny_result), fixed = T)

aux = unlist(strsplit(  gsub(".csv", "", p_file, fixed = T) , split="_" ))[-c(1,2)]
organism = aux[1]
top = as.numeric(aux[2])

# PLOTS -------------------------------------------------------------

heatmap_scores = function(df){
  
  paletteLength = 100
  myColor <- colorRampPalette(c("#99004C", "whitesmoke", "#0859A2"))(paletteLength)
  
  progenyBreaks <- c(seq(min(as.vector(df)), 0, 
                         length.out=ceiling(paletteLength/2) + 1),
                     seq(max(as.vector(df))/paletteLength, 
                         max(as.vector(df)), 
                         length.out=floor(paletteLength/2)))
  
  pheatmap(df,fontsize=14, 
           fontsize_row = 10, fontsize_col = 10, 
           color=myColor, breaks = progenyBreaks,
           angle_col = 45, treeheight_col = 0,  
           border_color = NA)
}

barplot_nes = function(df, smpl){
  
  df = df[, c("pathways", smpl)] %>%
    dplyr::rename(zscore = smpl) %>%
    dplyr::arrange(zscore) %>%
    dplyr::mutate(pathways = factor(pathways))
    
  title = paste("Sample/Contrast:", smpl, sep = " ")
  
  ggplot(df, aes(x = zscore, y = reorder(pathways, zscore) )) +
    geom_bar(aes(fill = zscore), stat = "identity") +
    scale_fill_gradient2(low = "#99004C", high = "#0859A2",
                         mid = "whitesmoke", midpoint = 0) +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(hjust = 1, size = 15, face= "bold"),
          axis.text.y = element_text(size = 15, face= "bold")) +
    ylab("Pathways") +
    xlab("z-scores") +
    ggtitle(title)
}

#adapted from progeny::progenyScatter
scater_pathway = function (df, weight_matrix, tittle) {
  
  #prepare data 
  names(df) <- c("ID", "stat")
  names(weight_matrix) <- c("ID", "weight")
  
  weight_matrix <- weight_matrix %>% 
    dplyr::filter(weight != 0)
  
  sub_df <- merge.data.frame(df, weight_matrix, by = "ID")
  sub_df$color <- "3"
  sub_df[(sub_df$weight > 0 & sub_df$stat > 0), "color"] <- "1"
  sub_df[(sub_df$weight > 0 & sub_df$stat < 0), "color"] <- "2"
  sub_df[(sub_df$weight < 0 & sub_df$stat > 0), "color"] <- "2"
  sub_df[(sub_df$weight < 0 & sub_df$stat < 0), "color"] <- "1"
  
  minstat <- min(df$stat)
  maxstat <- max(df$stat)
  
  # create scatterplot
  percentile <- ecdf(df$stat)
  sub_df[(percentile(sub_df$stat) < 0.95 & percentile(sub_df$stat) > 0.05), 1] <- NA
  
  
  scatterplot <- ggplot(sub_df, aes(x = weight, y = stat, color = color)) + 
    geom_point() + 
    scale_colour_manual(values = c("#99004C", "#0859A2", "grey")) + #"red", "royalblue3"
    geom_label_repel(aes(label = ID)) + 
    theme_light() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text.x = element_text(hjust = 1, size = 15, face= "bold"),
          axis.text.y = element_text(size = 15, face= "bold"),
          legend.position = "none") +
    expand_limits(x = 0, y = 0) +
    geom_vline(xintercept = 0, linetype = "dotted") + 
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_y_continuous(breaks = scales::extended_breaks()) +
    scale_x_continuous(breaks = scales::extended_breaks())#+ 
  #labs(x = title, y = statName)
  
  
  #create Histogram with input data
  histo <- ggplot(df, aes(x = stat, fill = "")) + 
           geom_density() + 
           coord_flip() + 
           scale_fill_manual(values = c("#dbdcdb")) + 
           xlim(layer_scales(scatterplot)$y$range$range) +
           #expand_limits(x = 0, y = 0) + #xlim(minstat, maxstat) + 
           theme_light() + 
           theme(legend.position = "none",
                  axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                  axis.title.y = element_blank(), axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(), panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(), panel.border = element_blank(),
                  axis.title = element_text(face = "bold", size = 12))
      
  plot_grid(scatterplot, histo, align = "hv", nrow = 1)

  
  }
