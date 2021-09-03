# PLOTS -------------------------------------------------------------

# Dorothea and KinAct ----------------------------------------------------------
barplot_nes_dorothea = function(df, smpl, nHits) {
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
      mid = "whitesmoke",
      midpoint = 0
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(
        hjust = 1,
        size = 10,
        face = "bold"
      ),
      axis.text.y = element_text(face = "bold")
    ) +
    ylab("") +
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
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(
        hjust = 1,
        size = 10,
        face = "bold"
      ),
      axis.text.y = element_text(face = "bold")
    ) +
    scale_fill_manual(values = c("#99004C", "#0859A2"),
                      drop = F) +
    theme(aspect.ratio = c(1)) +
    ggtitle(selTF)
  
}

plot_network = function(data, footprint_result, regulon, sample, selected_hub, number_targets){
  colnames(regulon)[1] = "hub"
  
  #select only targets of the hub
  targets_of_hub = regulon %>% 
    dplyr::filter(hub == selected_hub) %>%
    dplyr::pull(target) %>%
    unique()
  
  # the activity of the hub from the footprint result
  hub_activity = footprint_result %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "hub") %>%
    dplyr::filter(hub == selected_hub) %>%
    dplyr::pull(!!as.name(sample))
  
  # get the activity of the targets from the data 
  nodes = data %>%
    tibble::rownames_to_column(var = "id") %>%
    dplyr::select(id, !!as.name(sample)) %>%
    dplyr::filter(id %in% targets_of_hub) %>%
    dplyr::arrange(desc(abs(!!as.name(sample)))) %>%
    dplyr::slice(1:number_targets) %>%
    rbind(.,c(selected_hub, hub_activity)) %>%
    dplyr::mutate(
      color = dplyr::case_when(
        !!as.name(sample) >= 0 ~ "#0859A2",
        !!as.name(sample) < 0 ~ "#99004C",
      )) %>%
    dplyr::mutate(label = id)

  # get the network from the regulon
  edges = regulon %>%
    dplyr::filter(target %in% nodes$id & hub == selected_hub) %>%
    dplyr::select(hub, mor, target) %>%
    dplyr::rename(from = hub, sign = mor, to = target) %>%
    dplyr::mutate(color = dplyr::case_when(sign == 1 ~ '#0578F0',
                                           sign == -1 ~ '#F20404',
                                           sign == 0 ~ '#777777'))
  
  # network aesthetics
  title = paste0(selected_hub, " for ", sample)
  
  # legends
  ledges <- data.frame(color = c("#0578F0", "#F20404"),
                       label = c("activation", "inhibition"), 
                       arrows = c("to", "to"),
                       font.align = "top")
  
  lnodes <- data.frame(label = c("Upregulated", "Downregulated"),
                       color = c("#0859A2", "#99004C"),
                       shape = c("circle", "circle"))
  # network  
  visNetwork::visNetwork(nodes, edges, main = title) %>% 
    visEdges(arrows = "to")
}

heatmap_scores = function(df) {
  paletteLength = 100
  myColor <-
    colorRampPalette(c("#99004C", "whitesmoke", "#0859A2"))(paletteLength)
  
  if(nrow(df) < 2){dendrogram = "column"} else{dendrogram = "both"}
  heatmaply::heatmaply(df, colors = myColor, dendrogram = dendrogram)
}

# Progeny -----------------------------------------------------------

barplot_nes_progeny = function(df, smpl) {
  df = df[, c("pathways", smpl)] %>%
    dplyr::rename(zscore = smpl) %>%
    dplyr::arrange(zscore) %>%
    dplyr::mutate(pathways = factor(pathways))
  
  title = paste("Sample/Contrast:", smpl, sep = " ")
  
  ggplot(df, aes(x = zscore, y = reorder(pathways, zscore))) +
    geom_bar(aes(fill = zscore), stat = "identity") +
    scale_fill_gradient2(
      low = "#99004C",
      high = "#0859A2",
      mid = "whitesmoke",
      midpoint = 0
    ) +
    theme_minimal() +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(
        hjust = 1,
        size = 15,
        face = "bold"
      ),
      axis.text.y = element_text(size = 10, face = "bold")
    ) +
    ylab("Pathways") +
    xlab("z-scores") +
    ggtitle(title)
}

#adapted from progeny::progenyScatter
scater_pathway = function (df, weight_matrix, title) {
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
  
  # create scatterplot
  percentile <- ecdf(df$stat)
  sub_df[(percentile(sub_df$stat) < 0.95 &
            percentile(sub_df$stat) > 0.05), 1] <- NA
  
  scatterplot <-
    ggplot(sub_df, aes(x = weight, y = stat, color = color)) +
    geom_point() +
    scale_colour_manual(values = c("#99004C", "#0859A2", "grey")) + #"red", "royalblue3"
    geom_label_repel(aes(label = ID)) +
    theme_light() +
    theme(
      axis.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(
        hjust = 1,
        size = 12,
        face = "bold"
      ),
      axis.text.y = element_text(face = "bold", size = 15),
      legend.position = "none"
    ) +
    xlab("Progeny weights") +
    ylab("Gene measurement") +
    expand_limits(x = 0, y = 0) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_y_continuous(breaks = scales::extended_breaks()) +
    scale_x_continuous(breaks = scales::extended_breaks())
  
  #create density with input data
  density_gene <- ggplot(df, aes(x = stat)) +
    geom_density() +
    coord_flip() +
    scale_fill_manual(values = c("#dbdcdb")) +
    xlim(layer_scales(scatterplot)$y$range$range) +
    theme_light() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(face = "bold", size = 15)
    )
  
  #create density with weights
  density_prog <- ggplot(weight_matrix, aes(x = weight)) +
    geom_density() +
    # scale_fill_manual(values = c("#dbdcdb")) +
    xlim(layer_scales(scatterplot)$x$range$range) +
    theme_void() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(face = "bold", size = 15) 
    )  +
    ggtitle(title)
  
  density_prog + 
    patchwork::plot_spacer() + 
    scatterplot + 
    density_gene + 
    patchwork::plot_layout(ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))
}

# CARNIVAL -----------------------------------------------------------
barplot_pea <- function(pea, threshold_adjpval = 0.05, n_paths = 10){
  
  ggdata = pea %>% 
    dplyr::rename(pvalue = `p-value`, AdjPvalu = `Adjusted p-value`) %>%
    dplyr::filter(AdjPvalu <= threshold_adjpval) %>% 
    dplyr::arrange(AdjPvalu) %>%
    dplyr::slice(1:n_paths)

  col_called = setdiff(colnames(ggdata), c("pvalue", "AdjPvalu"))
  colnames(ggdata)[which(colnames(ggdata)==col_called)] = "pathway"
  
  ggdata %>%
  ggplot2::ggplot(aes(y = reorder(stringr::str_wrap(pathway, width = 20), -AdjPvalu), x = AdjPvalu)) +
    ggplot2::geom_segment(aes(y = reorder(stringr::str_wrap(pathway, width = 20), -AdjPvalu),
                              yend = reorder(stringr::str_wrap(pathway, width = 20), -AdjPvalu),
                              x = AdjPvalu, xend = 100),
                          color = "gray", lwd = 1) +
    ggplot2::geom_point(size = 4, pch = 21, bg = 4, col = 1) +
    ggplot2::scale_x_continuous(trans = ggforce::trans_reverser('log10'),#scales::log10_trans(),#ggforce::trans_reverser('log10'),
                                breaks = scales::trans_breaks("log10", function(x) 10^x),
                                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    ggplot2::xlab("Adjusted p-value") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal(base_size = 12)
  
}

volcano_pea <- function(pea, nodAtt, threshold_adjpval = 0.05, n_paths = 10, n_genes = 4){
  
  ggdata = plyr::join_all(pea, by = colnames(pea$annot)[2]) %>% 
    dplyr::rename(pvalue = `p-value`, AdjPvalu = `Adjusted p-value`, Node = genesymbol) %>%
    dplyr::inner_join(nodAtt, by = "Node") %>%
    dplyr::mutate(across(c(ZeroAct, UpAct, DownAct, AvgAct), as.numeric))
  
  xlimAbs <- ceiling(max(abs(ggdata$AvgAct), na.rm = T))
  ylimAbs <- ceiling(max(abs(log10(ggdata$AdjPvalu)), na.rm = T))
  
  vAss <- 0.5
  hAss <- threshold_adjpval
  
  xneg <- function(x) abs(hAss + 0.2 + x/(x + vAss))
  xpos <- function(x) abs(hAss + 0.2 + x/(x - vAss))
  
  ggplot(ggdata, aes(x = AvgAct, y = -log10(AdjPvalu) )) + # , color = supra_pathway
    geom_point(alpha = 0.7, na.rm = F, colour = "#918D8D") +
    geom_point(data = get_labels(ggdata, ceiling(n_genes/2), n_paths, threshold_adjpval),
               aes(x = AvgAct, y = -log10(AdjPvalu), color = !!as.name(colnames(ggdata)[2])), 
               alpha = 0.7, na.rm = F) +
    stat_function(fun = xneg, xlim = c(-xlimAbs, -vAss),
                  color = "black", alpha = 0.7) +
    stat_function(fun = xpos, xlim = c(vAss, xlimAbs),
                  color = "black", alpha = 0.7) +
    ggrepel::geom_label_repel(data = get_labels(ggdata, ceiling(n_genes/2), n_paths, threshold_adjpval), 
                              aes(x = AvgAct, y = -log10(AdjPvalu),
                                  color = !!as.name(colnames(ggdata)[2]), label = Node),
                              show.legend = F, inherit.aes = F) +
    scale_y_continuous(limits = c(0, ylimAbs), 
                       expand = c(0.01, 0.01),
                       breaks = seq(floor(min(-log10(ggdata$AdjPvalu), na.rm = T)), ceiling(max(-log10(ggdata$AdjPvalu), na.rm = T)), 1),
                       labels = scales::math_format(10^-.x)
    )+
    annotation_logticks(sides = "lr") +
    xlab("Node activity") + ylab("Adjusted p-value")  +
    theme_bw(base_size = 15)
  
}