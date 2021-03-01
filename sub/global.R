library(shiny)
library(shinyWidgets)
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
example_result_carnival = readRDS("data/examples/carnival_result_celline_SIDM00194.rds")

# load data
rwth_colors_df = get(load("data/misc/rwth_colors.rda"))

# kinact
kinact_regulon_human = readRDS("data/models/kinact_regulon_human.rds")


# ANALYSIS -------------------------------------------------------------

run_progeny <- function(data, organism = "Human", top = 100, perm = 100, ...){
  # based on organism, we load the correct dataset
  if(input$select_organism == "Homo sapiens"){
    organism = "Human"
    data(model_human_full, package = "progeny")
  }else if(input$select_organism == "Mus musculus"){
    organism = "Mouse"
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
      dplyr::rename(source = source_genesymbol, interaction = consensus_stimulation, target = target_genesymbol)
    
  }else if(net_type == "protein"){
    cNET <- cNET %>%
      dplyr::select(source, consensus_stimulation, target) %>%
      dplyr::rename(interaction = consensus_stimulation)
  }
  
  # if true, keep the complexes
  if(complx){
    cNET <- cNET %>%
      dplyr::mutate(source = gsub(":", "_", source)) %>%
      dplyr::mutate(source = gsub(":", "_", target))
    
  }else{
    cNET <- cNET %>%
      dplyr::filter(! (grepl(":", source, fixed = T) | grepl(":", target, fixed = T)))
  }
  
  return(cNET)
}

run_carnival <- function(data, net = NULL, net_type = "gene", 
                         dorothea = NULL, progeny = NULL,
                         ini_nodes = "all_inputs", ...){
  # load network
  if( is.null(net) ){
    cNET = get_network(net_type = "gene", complx = T)
  }else{
    cNET = read.delim(net, sep = "\t")
    colnames(cNET) = c('source', 'interaction', 'target')
  }
  
  # load dorothea
  if( is.null(dorothea) ){
    tf_activities = run_dorothea(data, organism = "Human", confidence_level, minsize = 5, method = 'none', ...)
  }else{
    tf_activities = read.delim(net, sep = "\t")
    
  }
  
  # load progeny
  if( is.null(progeny) ){
    progeny_scores = run_progeny(data, organism = "Human", ...)
  }else{
    progeny_scores = read.delim(net, sep = "\t")
    
  }
  
  # initial nodes
  if(!is.null(ini_nodes)){
    
    if(ini_nodes == "all_inputs"){
      # get initial nodes
      ini_nodes = base::setdiff(cNET$source, cNET$target)
    }
    
    iniciators = base::data.frame(base::matrix(data = NaN, nrow = 1, ncol = length(ini_nodes)), stringsAsFactors = F)
    colnames(iniciators) = ini_nodes
    
  }
  
  
  # run CARNIVAL
  carnival_result = runCARNIVAL( inputObj = iniciators,
                                 measObj = tfList$t, 
                                 netObj = cNET, 
                                 weightObj = progenylist$score, 
                                 solverPath = "/Applications/CPLEX_Studio129/cplex/bin/x86-64_osx/cplex", 
                                 solver = "cplex",
                                 timelimit=7200,
                                 mipGAP=0,
                                 poolrelGAP=0 )
  
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
