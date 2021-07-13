# support functions ---------------------------------------------------

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

pathEnreach <- function(nodeAtt, database, select_collection = NULL){
  
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
        dplyr::filter(collection %in% select_collection)
      
      
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

#function to handle if the data come from example or upload to make it suitable for the analysis
progessDATA <- function(data, contrast_data = F, upload_expr, type_analysis){
  
  if( any( contrast_data | all(!is.null(upload_expr) & !is.null(type_analysis)) ) ){
    upcon = T
    
    if(!is.null(type_analysis)){
      if(type_analysis == "multi"){
        upcon = F
      }
    }
    
    if(any(contrast_data | upcon)){
      data = data %>%
        dplyr::select(t) %>%
        unique.data.frame()
    }
  }
  
  return(data)
  
}