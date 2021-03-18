library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(tidyverse)
library(viper)
library(ggrepel)
library(DT)
library(furrr)
library(pheatmap)
library(shinyjs)
library(ggExtra)
library(tidygraph)
library(ggraph)
library(broom)
library(dorothea)
library(progeny)
library(CARNIVAL)
library(OmnipathR)
library(visNetwork)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# load examples
# carnival_result = readRDS("data/examples/carnival_result_celline_SIDM00194.rds")
# carnival_result$nodesAttributes = as.data.frame(carnival_result$nodesAttributes)
# carnival_result$weightedSIF = as.data.frame(carnival_result$weightedSIF)

# load data
# rwth_colors_df = get(load("data/misc/rwth_colors.rda"))

# kinact
# kinact_regulon_human = readRDS("data/models/kinact_regulon_human.rds")


# ANALYSIS -------------------------------------------------------------

run_progeny <- function(data, organism = "Human", top = 100, perm = 100, ...){
  # based on organism, we load the correct dataset
  if(organism == "Human"){
    data(model_human_full, package = "progeny")
  }else if(organism == "Mouse"){
    data(model_mouse_full, package = "progeny")
  }
  
  #run PROGENy
  
  #set seed to always get the same result
  set.seed(7895874)
  
  progeny_scores <- data %>%
    as.matrix() %>%
    progeny::progeny(., z_scores = FALSE, 
                     organism = organism,
                     top = top,
                     perm = perm)
  return(progeny_scores)
}

run_dorothea <- function(dorothea_matrix, organism = "Human", confidence_level, minsize = 5, method = 'none', ...){
  # based on organism, we load the correct dataset
  if(organism == "Human"){
    
    data(dorothea_hs, package = "dorothea")
    regulons <- dorothea_hs %>%
      dplyr::filter(confidence %in% confidence_level)
    
  }else if(organism == "Mouse"){
    
    data(dorothea_mm, package = "dorothea")
    regulons <-  dorothea_mm %>%
      dplyr::filter(confidence %in% confidence_level)
    
  }
  #run DOROTHEA
  activity_scores <- dorothea::run_viper(dorothea_matrix, regulons,
                                         options =  list(minsize = minsize,
                                                         method = method,
                                                         eset.filter = FALSE,
                                                         cores = 1,
                                                         verbose = FALSE, 
                                                         nes = TRUE))
  
  return(activity_scores)
}

get_network <- function(net_type = "gene", complx = T){
  omniR = OmnipathR::import_Omnipath_Interactions()
  
  #consensus sign and direction
  cNET <- omniR %>% 
    dplyr::filter(consensus_direction == 1 &
                    (consensus_stimulation == 1 | consensus_inhibition == 1)) %>%
    dplyr::mutate(consensus_stimulation = dplyr::if_else(consensus_stimulation == 0, -1, 1)) %>%
    dplyr::mutate(consensus_inhibition = dplyr::case_when(consensus_inhibition == 1 ~ -1,
                                                          consensus_inhibition == 0 ~ 1)) %>%
    dplyr::filter(consensus_stimulation==consensus_inhibition)
  
  # select gene or protein network
  if(net_type == "gene"){
    cNET <- cNET %>%
      dplyr::select(source_genesymbol, consensus_stimulation, target_genesymbol) %>%
      dplyr::rename(source = source_genesymbol, interaction = consensus_stimulation, target = target_genesymbol) %>%
      unique.data.frame()
    
  }else if(net_type == "protein"){
    cNET <- cNET %>%
      dplyr::select(source, consensus_stimulation, target) %>%
      dplyr::rename(interaction = consensus_stimulation) %>%
      unique.data.frame()
  }
  
  # if true, keep the complexes
  if(complx){
    cNET <- cNET %>%
      dplyr::mutate(source = gsub(":", "_", source)) %>%
      dplyr::mutate(target = gsub(":", "_", target))
    
  }else{
    cNET <- cNET %>%
      dplyr::filter(! (grepl(":", source, fixed = T) | grepl(":", target, fixed = T)))
  }
  
  return(cNET)
}

run_carnival <- function(data, net = NULL, dorothea = NULL, progeny = NULL,
                         ini_nodes = "all_inputs", 
                         solver =  list(spath = NULL , solver = "lpSolve"), ...){
  # load network
  if( is.list(net) ){
    cNET = get_network(net_type = net$net_type, complx = net$net_complex)
  }else{
    cNET = read.delim(net, sep = "\t")
    colnames(cNET) = c('source', 'interaction', 'target')
  }
  
  # load dorothea
  if( is.list(dorothea) ){
      
    tf_activities = run_dorothea(dorothea_matrix = data, 
                                 organism = dorothea$organism, 
                                 confidence_level = dorothea$confidence_level, 
                                 minsize = dorothea$minsize, 
                                 method = dorothea$method)
  }else{
    tf_activities = read.delim(dorothea, sep = "\t")
  }

  # load progeny
  if( is.list(progeny) ){
    
    progeny_scores = run_progeny(data, 
                                 organism = progeny$organism, 
                                 top = progeny$top, 
                                 perm = progeny$perm)
  }else{
    progeny_scores = read.delim(progeny, sep = "\t")
  }
  
  #dorothea
  tfList = generateTFList(tf_activities, top = 50, access_idx = 1:ncol(tf_activities))
  
  #progeny
  load(file = system.file("progenyMembers.RData",package="CARNIVAL"))
  
  progenylist = assignPROGENyScores(progeny_scores,
                                    progenyMembers = progenyMembers, 
                                    id = "gene",
                                    access_idx = 1:nrow(progeny_scores))

  # initial nodes
  if( ini_nodes != "inverse" ){
    
    if(ini_nodes == "all_inputs"){
      ini_nodes = base::setdiff(cNET$source, cNET$target)
    }else if(ini_nodes == "up"){
      ini_nodes = read.delim(ini_nodes, sep = "\t")
    }
      
    iniciators = base::data.frame(base::matrix(data = NaN, nrow = 1, ncol = length(ini_nodes)), stringsAsFactors = F)
    colnames(iniciators) = ini_nodes
      
  }else{iniciators = NULL}

  # run CARNIVAL
  carnival_result = runCARNIVAL( inputObj = iniciators,
                                 measObj = tfList[[1]], 
                                 netObj = cNET, 
                                 weightObj = progenylist[[1]], 
                                 solverPath = solver$spath, 
                                 solver = solver$solver,
                                 timelimit = 7200,
                                 mipGAP = 0,
                                 poolrelGAP = 0 )
  
  carnival_result$weightedSIF = carnival_result$weightedSIF %>%
    as.data.frame() %>% 
    tibble::tibble() %>%
    dplyr::mutate(across(c(Sign, Weight), as.numeric))
    
  carnival_result$nodesAttributes = carnival_result$nodesAttributes %>%
    as.data.frame() %>% 
    tibble::tibble() %>%
    dplyr::mutate(across(c(ZeroAct, UpAct, DownAct, AvgAct), as.numeric))
  
  return(carnival_result)
  
}

assignPROGENyScores <- function (progeny = progeny, progenyMembers = progenyMembers, 
                                 id = "gene", access_idx = 1) 
{
  if (id == "uniprot") {
    idx <- which(names(progenyMembers) == "uniprot")
    progenyMembers <- progenyMembers[[idx]]
  }
  else {
    idx <- which(names(progenyMembers) == "gene")
    progenyMembers <- progenyMembers[[idx]]
  }
  members <- matrix(data = , nrow = 1, ncol = 2)
  pathways <- colnames(progeny)
  ctrl <- intersect(x = access_idx, y = 1:nrow(progeny))
  if (length(ctrl) == 0) {
    stop("The indeces you inserted do not correspond to \n              the number of rows/samples")
  }
  for (ii in 1:length(pathways)) {
    mm <- progenyMembers[[which(names(progenyMembers) == 
                                  pathways[ii])]]
    for (jj in 1:length(mm)) {
      members <- rbind(members, c(pathways[ii], mm[jj]))
    }
  }
  members <- members[-1, ]
  scores <- matrix(data = , nrow = nrow(progeny), ncol = nrow(members))
  colnames(scores) <- members[, 2]
  rownames(scores) <- rownames(progeny)
  members <- unique(members)
  for (i in 1:ncol(scores)) {
    for (j in 1:nrow(scores)) {
      scores[j, i] <- as.numeric(progeny[j, members[which(members[, 
                                                                  2] == colnames(scores)[i]), 1]])
    }
  }
  pxList <- list()
  for (ii in 1:length(access_idx)) {
    pxList[[length(pxList) + 1]] <- as.data.frame(t(as.matrix(scores[access_idx[ii], 
                                                                     ])))
  }
  names(pxList) <- rownames(progeny)[ctrl]
  return(pxList)
}

generateTFList <- function (df = df, top = 50, access_idx = 1) 
{
  if (top == "all") {
    top <- nrow(df)
  }
  if (top > nrow(df)) {
    warning("Number of to TF's inserted exceeds the number of actual TF's in the data frame. All the TF's will be considered.")
    top <- nrow(df)
  }
  ctrl <- intersect(x = access_idx, y = 1:ncol(df))
  if (length(ctrl) == 0) {
    stop("The indeces you inserted do not correspond to the number of columns/samples")
  }
  returnList <- list()
  for (ii in 1:length(ctrl)) {
    tfThresh <- sort(x = abs(df[, ctrl[ii]]), decreasing = TRUE)[1:top]
    currDF <- df[which(rownames(df)%in%names(tfThresh)), ctrl[ii]]
    returnList[[ colnames(df)[ctrl[ii]] ]] <- as.data.frame(t(currDF))
  }
  return(returnList)
}

# PLOTS -------------------------------------------------------------

# Dorothea ----------------------------------------------------------
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

# Progeny -----------------------------------------------------------

heatmap_scores = function(df) {
  paletteLength = 100
  myColor <-
    colorRampPalette(c("#99004C", "whitesmoke", "#0859A2"))(paletteLength)
  
  progenyBreaks <- c(
    seq(min(as.vector(df)), 0,
        length.out = ceiling(paletteLength / 2) + 1),
    seq(
      max(as.vector(df)) / paletteLength,
      max(as.vector(df)),
      length.out = floor(paletteLength / 2)
    )
  )
  
  pheatmap(
    df,
    fontsize = 14,
    fontsize_row = 10,
    fontsize_col = 10,
    color = myColor,
    breaks = progenyBreaks,
    angle_col = 45,
    treeheight_col = 0,
    border_color = NA
  )
}

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
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(
        hjust = 1,
        size = 15,
        face = "bold"
      ),
      axis.text.y = element_text(size = 15, face = "bold")
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
      axis.title = element_text(face = "bold", size = 12),
      axis.text.x = element_text(
        hjust = 1,
        size = 15,
        face = "bold"
      ),
      axis.text.y = element_text(size = 15, face = "bold"),
      legend.position = "none"
    ) +
    expand_limits(x = 0, y = 0) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_y_continuous(breaks = scales::extended_breaks()) +
    scale_x_continuous(breaks = scales::extended_breaks()) +
    ggtitle(title)
  
  #create Histogram with input data
  histo <- ggplot(df, aes(x = stat, fill = "")) +
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
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(face = "bold", size = 12)
    )
  
cowplot::plot_grid(scatterplot, histo, align = "hv", nrow = 1)

}