library(shiny)
library(shinyWidgets)
library(shinyFiles)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(reshape2)
library(DT)
library(ggplot2)
library(ggrepel)
library(plotly)
library(heatmaply)
library(patchwork)
library(visNetwork)
library(dorothea)
library(progeny)
library(CARNIVAL)
library(OmnipathR)
library(cosmosR)
library(org.Hs.eg.db)
library(AnnotationDbi)

# shiny options
enableBookmarking(store = "server")
options(shiny.maxRequestSize=30*1024^2)

# Load models
kinact_regulon_human = readRDS("data/models/kinact_regulon_human_symbol.rds")

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
    progeny::progeny(., z_scores = TRUE, 
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

pathEnreach <- function(nodeAtt, database){
  
  gsea_sets = list(success = nodeAtt %>% 
                     dplyr::filter(ZeroAct != 100) %>%
                     dplyr::select(Node) %>%
                     dplyr::pull(),
                   bg = nodeAtt$Node,
                   gs = database)
  
  gsea_analysis = piano::runGSAhyper(genes = gsea_sets$success,
                                     universe = gsea_sets$bg,
                                     gsc = piano::loadGSC(gsea_sets$gs))
  
  gsea_analysis_df = gsea_analysis$resTab %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = colnames(database)[2]) %>%
    dplyr::select(1:3) %>%
    as.data.frame() %>% 
    tibble::tibble() %>%
    dplyr::arrange((!!as.name('Adjusted p-value'))) 
  
  return(list( annot = database, pea = gsea_analysis_df ))  
}

#' run COSMOS
#' 
#' This is a wrapper to run the main steps of the cosmosR pipeline all in one.
#' It performs the pre-processing, carnival optimisation and output formatting 
#' for both forward and backward cosmos runs.
#' @param layer_1 numerical vector, where names are nodes identifiers as
#' in the PKN and values are from \{1, 0, -1\}. Continuous data will be 
#' discretized using the \code{\link{sign}} function.
#' @param layer_2 numerical vector, where names are nodes identifiers as
#' in the PKN and values are from \{1, 0, -1\}. Continuous data will be 
#' discretized using the \code{\link{sign}} function.
#' @param RNA_data numerical vector that represents the 
#' results of a differential gene expression analysis. Names are gene
#' names using EntrezID starting with an X and values are log fold change or
#'  t-values.  \code{\link{convert_genesymbols_to_entrezid}} can be used for
#'  conversion.  We use the \dQuote{\code{diff_exp_threshold}} parameter to decide
#'  which genes changed significantly.  Genes with NA values are considered none
#'  expressed and they will be removed from the TF-gene expression interactions. 
#' @param PKN prior knowledge network (PKN).
#' @param runtime a numeric vector with 4 values, representing the runtime of carnival for 
#' 1) the pre-forward run, 2) the forward run, 3) the pre-backward run and 4) the backward run
#' 1) and 3) can be filled with any number and won't be used if \dQuote{\code{filter_tf_gene_interaction_by_optimization}}
#' is set to FALSE
#' @param solver name of the solver to be used ("cplex", "cbc" or "lpSolve")
#' @param solver_path full path to the solver executable if cplex or cbc are used
#' @param diff_exp_threshold threshold parameter (default 1) used to binarize
#'  the values of diff_expression_data. 
#' @param maximum_network_depth integer > 0 (default: 8). Nodes that are further 
#' than \dQuote{\code{maximum_network_depth}} steps from the signaling nodes on 
#' the directed graph of the PKN are considered non-reachable and are removed. 
#' @param remove_unexpressed_nodes if TRUE (default) removes nodes from the PKN 
#' that are not expressed.
#' @param filter_tf_gene_interaction_by_optimization (default:TRUE), if TRUE then runs 
#' a network optimization that estimates TF activity not included in the inputs
#' and checks the consistency between the estimated activity and change in gene 
#' expression. Removes interactions where TF and gene expression are inconsistent 
#' @param full_loop if TRUE, both forward and backward run will be performed.
#' if FALSE, only forward run will be performed
#' @return list with two elements: the sif network and the node attributes
run_COSMOS <- function(layer_1, 
                       layer_2, 
                       RNA_data, 
                       PKN, 
                       runtime = c(1800, 7200, 1800, 7200), 
                       solver = "lpsolve", 
                       solver_path = NULL,
                       maximum_network_depth = 8,
                       remove_unexpressed_nodes = T,
                       filter_tf_gene_interaction_by_optimization = F,
                       diff_exp_threshold = 1,
                       full_loop = F)
{
  #layer_1
  if(!class(layer_1) == "numeric")
  {
    layer_1_vec <- as.numeric(layer_1[1,])
    names(layer_1_vec) <- names(layer_1)
  } else
  {
    layer_1_vec <- layer_1
  }
  
  
  #layer_2
  if(!class(layer_2) == "numeric")
  {
    layer_2_vec <- as.numeric(layer_2[1,])
    names(layer_2_vec) <- names(layer_2)
  } else
  {
    layer_2_vec <- layer_2
  }
  
  #RNA_input
  if(!class(layer_2) == "numeric")
  {
    RNA_input <- RNA_data[,"t"]
    names(RNA_input) <- paste0("X",RNA_data$ID)
  } else
  {
    RNA_input <- RNA_data
  }
  #filter inputs
  layer_1_vec <- layer_1_vec[names(layer_1_vec) %in% PKN$source | names(layer_1_vec) %in% PKN$target]
  layer_2_vec <- layer_2_vec[names(layer_2_vec) %in% PKN$source | names(layer_2_vec) %in% PKN$target]
  
  #setup carnival options
  my_options <- default_CARNIVAL_options()
  
  if(!is.null(solver_path))
  {
    my_options$solverPath <- solver_path
  }
  my_options$solver <- solver
  
  my_options$timelimit <- runtime[1]
  
  prerun_forward <- preprocess_COSMOS_signaling_to_metabolism(meta_network = PKN, 
                                                              signaling_data = layer_1_vec,
                                                              metabolic_data = layer_2_vec,
                                                              diff_expression_data = RNA_input,
                                                              maximum_network_depth = maximum_network_depth,
                                                              remove_unexpressed_nodes = remove_unexpressed_nodes,
                                                              filter_tf_gene_interaction_by_optimization = filter_tf_gene_interaction_by_optimization,
                                                              diff_exp_threshold = diff_exp_threshold,
                                                              CARNIVAL_options = my_options)#
  
  my_options$timelimit <- runtime[2]
  
  result_for <- run_COSMOS_signaling_to_metabolism(data = prerun_forward,
                                                   CARNIVAL_options = my_options)
  
  data("metabolite_to_pubchem")
  data("omnipath_ptm")
  
  result_for <- format_COSMOS_res(result_for,
                                  metab_mapping = metabolite_to_pubchem,
                                  measured_nodes = unique(c(names(layer_2_vec),
                                                            names(layer_1_vec))),
                                  omnipath_ptm = omnipath_ptm)
  
  ## BACKWARD RUN
  if(full_loop)
  {
    my_options$timelimit <- runtime[3]
    
    prerun_backward <- preprocess_COSMOS_metabolism_to_signaling(meta_network = PKN,
                                                                 signaling_data = layer_1_vec,
                                                                 metabolic_data = layer_2_vec,
                                                                 diff_expression_data = RNA_input,
                                                                 maximum_network_depth = maximum_network_depth,
                                                                 remove_unexpressed_nodes = remove_unexpressed_nodes,
                                                                 filter_tf_gene_interaction_by_optimization = filter_tf_gene_interaction_by_optimization,
                                                                 diff_exp_threshold = diff_exp_threshold,
                                                                 CARNIVAL_options = my_options)#
    
    my_options$timelimit <- runtime[4]
    
    result_back <- run_COSMOS_metabolism_to_signaling(data = prerun_backward,
                                                      CARNIVAL_options = my_options)
    
    result_back<- format_COSMOS_res(result_back,
                                    metab_mapping = metabolite_to_pubchem,
                                    measured_nodes = unique(c(names(layer_2_vec),
                                                              names(layer_1_vec))),
                                    omnipath_ptm = omnipath_ptm)
    
    ###Merge forward and backward networks
    
    full_sif <- as.data.frame(rbind(result_for[[1]], result_back[[1]]))
    full_attributes <- as.data.frame(rbind(result_for[[2]], result_back[[2]]))
    
    full_sif <- unique(full_sif)
    full_attributes <- unique(full_attributes)
    
    return(list("full_sif" = full_sif, "full_attributes" = full_attributes))
  }
  return(result_for)
}

