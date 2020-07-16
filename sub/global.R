
#initialize

library(shiny)
library(DT)
library(tidyverse)
library(dorothea)
library(ggplot2)
library(dplyr)
library(reshape2)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# Load data
dorothea_result = read.csv( "data/examples/dorothea_scores_ABC.csv", row.names = 1 )
data(dorothea_hs, package = "dorothea")


# PLOTS -------------------------------------------------------------

barplot_nes = function(df, smpl, nHits){
  
  df = df[, c("GeneID", smpl)] %>%
    dplyr::rename(NES = smpl) %>%
    dplyr::top_n(nHits, wt = abs(NES)) %>%
    dplyr::arrange(NES) %>%
    dplyr::mutate(GeneID = factor(GeneID))
    
  title = paste("Sample/Contrast:", smpl, sep = " ")
  
  ggplot(df, aes(x = reorder(GeneID, NES), y = NES)) +
    geom_bar(aes(fill = NES), stat = "identity") +
    scale_fill_gradient2(low = "darkblue", high = "indianred",
                         mid = "whitesmoke", midpoint = 0) +
    theme_minimal() +
    theme(axis.title = element_text(face = "bold", size = 12),
          axis.text.x =
            element_text(angle = 45, hjust = 1, size =10, face= "bold"),
          axis.text.y = element_text(size =10, face= "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("Transcription Factors") +
    ylab("Normalized Enrichment scores (NES)") +
    ggtitle(title)
}

barplot_tf = function(df, selTF){

  df %>%
    rownames_to_column(var = "tf") %>%
    dplyr::filter(tf == selTF)  %>% 
    reshape2::melt() %>%
    arrange(value) %>%
    dplyr::mutate(variable = factor(variable, variable),
           effect = factor(sign(value), c(-1,1))) %>%
    ggplot(aes(x=variable, y=value, fill=effect)) +
    geom_col() +
    coord_flip() +
    labs( x = "Sample/Contrast:", y = "Normalized Enrichment scores (NES)") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_manual(values = rwth_color(c("magenta", "green")),
                      drop=F) +
    theme(aspect.ratio = c(1)) +
    ggtitle(paste0("TF: ", selTF))
  
}


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