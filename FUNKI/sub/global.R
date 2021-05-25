library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyBS)
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
library(promises)
library(future)
plan(multisession)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# Load models
kinact_regulon_human = readRDS("data/models/kinact_regulon_human_symbol.rds")

# load examples
# carnival_result = readRDS("data/examples/carnival_result_celline_SIDM00194.rds")
# carnival_result$nodesAttributes = as.data.frame(carnival_result$nodesAttributes)
# carnival_result$weightedSIF = as.data.frame(carnival_result$weightedSIF)
# carnival_result$nodesAttributes = carnival_result$nodesAttributes %>%
#   dplyr::filter(Node %in%union(carnival_result$weightedSIF$Node1,carnival_result$weightedSIF$Node2))


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

run_dorothea <- function(dorothea_matrix, organism = "Human", confidence_level = c("A", "B", "C"), minsize = 5, method = 'none', ...){
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

run_kinact <- function(data, organism = "Human", minsize = 5, method = 'none', ...){
  # based on organism, we load the correct dataset
    kinact_regulon_human = readRDS("data/models/kinact_regulon_human.rds") %>%
      dplyr::rename(tf = kinase)
    
  #run viper
  kinase_scores <- dorothea::run_viper(data, kinact_regulon_human,
                                         options =  list(minsize = minsize,
                                                         method = method,
                                                         eset.filter = FALSE,
                                                         cores = 1,
                                                         verbose = FALSE, 
                                                         nes = TRUE))
  
  return(kinase_scores)
}

get_network <- function(net_type = "gene", complx = T){
  omniR = OmnipathR::import_omnipath_interactions()
  
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
    cNET = read.delim(net, sep = ",")
    colnames(cNET) = c('source', 'interaction', 'target')
  }

  # load dorothea
  if( is.list(dorothea) ){
      
    tf_activities = run_dorothea(dorothea_matrix = data, 
                                 organism = dorothea$organism, 
                                 confidence_level = dorothea$confidence_level, 
                                 minsize = dorothea$minsize, 
                                 method = dorothea$method)
    #dorothea
    tfList = generateTFList(tf_activities, top = 50, access_idx = 1:ncol(tf_activities))[[1]]
    
  }else{
    tfList = read.delim(dorothea, sep = ",")
  }

  # load progeny
  if( is.list(progeny) ){
    
    #progeny
    load(file = system.file("progenyMembers.RData", package = "CARNIVAL"))
    
    progeny_scores = run_progeny(data, 
                                 organism = progeny$organism, 
                                 top = progeny$top, 
                                 perm = progeny$perm)
    
    progenylist = assignPROGENyScores(progeny_scores,
                                      progenyMembers = progenyMembers, 
                                      id = "gene",
                                      access_idx = 1:nrow(progeny_scores))[[1]]
    
  }else if (is.character(progeny)){
    progenylist = read.delim(progeny, sep = "\t")
  }else{progenylist = NULL}
  
  # initial nodes
  if( ini_nodes == "inverse" ){ 
    
    iniciators = NULL
    
  }else if(ini_nodes == "all_inputs"){
    
    ini_nodes = base::setdiff(cNET$source, cNET$target)
    iniciators = base::data.frame(base::matrix(data = NaN, nrow = 1, ncol = length(ini_nodes)), stringsAsFactors = F)
    colnames(iniciators) = ini_nodes
      
  }else{
    iniciators = read.delim(ini_nodes, sep = ",")
  }

  # run CARNIVAL
  carnival_result = runCARNIVAL( inputObj = iniciators,
                                 measObj = tfList, 
                                 netObj = cNET, 
                                 weightObj = progenylist, 
                                 solverPath = solver$spath, 
                                 solver = solver$solver,
                                 timelimit = 7200,
                                 mipGAP = 0,
                                 poolrelGAP = 0 )
  
  if(!is.null(carnival_result)){
    carnival_result$weightedSIF = carnival_result$weightedSIF %>%
      as.data.frame() %>% 
      tibble::tibble() %>%
      dplyr::mutate(across(c(Sign, Weight), as.numeric))
    
    carnival_result$nodesAttributes = carnival_result$nodesAttributes %>%
      as.data.frame() %>% 
      tibble::tibble() %>%
      dplyr::mutate(across(c(ZeroAct, UpAct, DownAct, AvgAct), as.numeric))
  }
  
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

pathEnreach <- function(nodeAtt, database, collection = NULL){
  
  if(database == 'Custom'){
    
    
    value = "pathway"
    
    annotations = read_tsv(collection)
    colnames(annotations) = c("genesymbol", "pathway")

  }else{
    
    annotations = OmnipathR::import_omnipath_annotations(
      resources = database,
      proteins = nodeAtt$Node,
      wide = TRUE)
    
    if(database == 'MSigDB'){
      value = "geneset"
      
      annotations = annotations %>%
        dplyr::filter(collection %in% collection)
      
      
    }else{value = "pathway"}
    
  }
  
  gsea_sets = list(success = nodeAtt %>% 
                     dplyr::filter(ZeroAct != 100) %>%
                     dplyr::select(Node) %>%
                     dplyr::pull(),
                   bg = nodeAtt$Node,
                   gs = annotations %>%
                     dplyr::select(genesymbol, !!as.name(value)))
  
    gsea_analysis = piano::runGSAhyper(genes = gsea_sets$success,
                                       universe = gsea_sets$bg,
                                       gsc = piano::loadGSC(gsea_sets$gs))
  
  gsea_analysis_df = gsea_analysis$resTab %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "pathway") %>%
    dplyr::select(1:3) %>%
    as.data.frame() %>% 
    tibble::tibble() %>%
    dplyr::arrange((!!as.name('Adjusted p-value'))) 
  
  return(list( annot = annotations, pea = gsea_analysis_df ))  
}

#Calculate all pathways
calculate_all_paths <- function(carnival_result){
  # create digraph
  DGs = igraph::graph_from_edgelist(carnival_result$weightedSIF %>% dplyr::select(Node1, Node2) %>% as.matrix(), directed = TRUE)
  # get inicial nodes
  iniciators = base::setdiff(carnival_result$weightedSIF$Node1, carnival_result$weightedSIF$Node2)
  # get effectors
  effectors = base::setdiff(carnival_result$weightedSIF$Node2, carnival_result$weightedSIF$Node1)
  
  paths = list()
  for (i in iniciators){
    for (j in effectors){
      aux = igraph::all_simple_paths(DGs, i, j)
      paths[[paste0(i,"_",j)]] = do.call(c, lapply(aux, igraph::as_ids))
    }
  }
  
  return(paths)
}

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
    tibble::rownames_to_column(var = "target") %>%
    dplyr::filter(target %in% targets_of_hub) %>%
    dplyr::select(target, !!as.name(sample)) %>%
    dplyr::rename(id = target) %>%
    dplyr::arrange(desc(abs(!!as.name(sample)))) %>%
    dplyr::slice(1:number_targets) %>%
    rbind(c(selected_hub, hub_activity)) %>%
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
    
    ggplot(ggdata, aes(y = reorder(pathway, AdjPvalu), x = -log10(AdjPvalu))) + 
    geom_bar(stat = "identity") +
    scale_x_continuous(
      expand = c(0.01, 0.01),
      limits = c(0, ceiling(max(-log10(ggdata$AdjPvalu)))),
      breaks = seq(floor(min(-log10(ggdata$AdjPvalu))), 
                   ceiling(max(-log10(ggdata$AdjPvalu))), 1),
      labels = scales::math_format(10^-.x)
    ) +
    annotation_logticks(sides = "bt") +
    theme_bw(base_size = 15) +
    theme(axis.title = element_text(),
          axis.text.y = element_text()) +
    xlab("Adjusted p-value") + ylab("")
    
}

volcano_pea <- function(pea, nodAtt, threshold_adjpval = 0.05, n_paths = 10, n_genes = 4){
  
  if(any("geneset" %in% colnames(pea$annot))){pea$annot = pea$annot %>% dplyr::rename(pathway = geneset)}
  
  ggdata = plyr::join_all(pea, by = "pathway") %>% 
    dplyr::rename(pvalue = `p-value`, AdjPvalu = `Adjusted p-value`, Node = genesymbol) %>%
    dplyr::inner_join(nodAtt, by = "Node") %>%
    dplyr::mutate(across(c(ZeroAct, UpAct, DownAct, AvgAct), as.numeric))
  
  xlimAbs <- ceiling(max(abs(ggdata$AvgAct)))
  ylimAbs <- ceiling(max(abs(log10(ggdata$AdjPvalu))))
  
  vAss <- 0.5
  hAss <- threshold_adjpval
  
  xneg <- function(x) abs(hAss + 0.2 + x/(x + vAss))
  xpos <- function(x) abs(hAss + 0.2 + x/(x - vAss))
  
  ggplot(ggdata, aes(x = AvgAct, y = -log10(AdjPvalu) )) + # , color = supra_pathway
    geom_point(alpha = 0.7, na.rm = F, colour = "#918D8D") +
    geom_point(data = get_labels(ggdata, ceiling(n_genes/2), n_paths, threshold_adjpval),
               aes(x = AvgAct, y = -log10(AdjPvalu), color = pathway), 
               alpha = 0.7, na.rm = F) +
    stat_function(fun = xneg, xlim = c(-xlimAbs, -vAss),
                  color = "black", alpha = 0.7) +
    stat_function(fun = xpos, xlim = c(vAss, xlimAbs),
                  color = "black", alpha = 0.7) +
    ggrepel::geom_label_repel(data = get_labels(ggdata, ceiling(n_genes/2), n_paths, threshold_adjpval), 
                              aes(x = AvgAct, y = -log10(AdjPvalu),
                                  color = pathway, label = Node),
                              show.legend = F, inherit.aes = F) +
    scale_y_continuous(limits = c(0, ylimAbs), 
                       expand = c(0.01, 0.01),
                       breaks = seq(floor(min(-log10(ggdata$AdjPvalu))), ceiling(max(-log10(ggdata$AdjPvalu))), 1),
                       labels = scales::math_format(10^-.x)
    )+
    annotation_logticks(sides = "lr") +
    xlab("CARNIVAL's activity") + ylab("Adjusted p-value")  +
    theme_bw(base_size = 15)
  
}

# support functions ---------------------------------------------------
#reverse function to allow a flip in the coord and be able to print the values in log scale
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

#get subset to add labels to
get_labels <- function(df, nlabel, npaths, threshold){
  pathways = df %>%
    dplyr::select(pathway, AdjPvalu) %>%
    unique.data.frame() %>%
    dplyr::filter(AdjPvalu <= threshold) %>%
    dplyr::arrange(AdjPvalu) %>%
    dplyr::pull(var = pathway)
  
  if(length(pathways) > npaths){
    pathways = pathways[1:npaths]
  }  
  
  aux_up = df %>% 
    dplyr::filter(AvgAct > 0, pathway %in% pathways) %>% 
    dplyr::group_by(pathway) %>% 
    dplyr::arrange(AdjPvalu, -AvgAct) %>%
    dplyr::slice(1:nlabel)
  
  aux_dwn = df %>% 
    dplyr::filter(AvgAct < 0, pathway %in% pathways) %>% 
    dplyr::group_by(pathway) %>% 
    dplyr::arrange(AdjPvalu, AvgAct) %>%
    dplyr::slice(1:nlabel)
  
  lbls = rbind.data.frame(aux_up, aux_dwn)
  
  return(lbls)
} 

