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
  col_called = setdiff(colnames(df), c("Node", "pvalue", "AdjPvalu", "ZeroAct", "UpAct", "DownAct", "AvgAct", "NodeType"))
  colnames(df)[which(colnames(df)==col_called)] = "pathway"
  pathways = df %>%
    dplyr::select(pathway, AdjPvalu) %>%
    unique.data.frame() %>%
    dplyr::filter(AdjPvalu <= threshold) %>%
    dplyr::arrange(AdjPvalu) %>%
    dplyr::pull(var = pathway)
  
  if(length(pathways) >= npaths){
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
  
  colnames(lbls)[which(colnames(lbls)=="pathway")] = col_called
  
  return(lbls)
}

#function to handle if the data come from example or upload to make it suitable for the analysis
progessDATA <- function(data, contrast_data = F, upload_expr, type_analysis, 
                        gene_id_type = NULL, running_method = "cosmos"){
  
  #change ids to correct ones
  if(all(!is.null(gene_id_type), gene_id_type != "Gene ID")){
    data = convert_genes_ids(data$ID, gene_id_type) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      tibble::rownames_to_column(var = "ID") %>% 
      dplyr::rename(newID = V1) %>%
      merge.data.frame(., data, by = "ID", all.y = T) %>%
      dplyr::select(!ID) %>%
      dplyr::rename(ID = newID) %>%
      tidyr::drop_na() %>%
      unique.data.frame()
  }
  
  if(running_method != "cosmos"){
    data = data %>%
      tibble::column_to_rownames(var = "ID")
  }
  
  #select ID and t columns for contrast analysis
  if( any( contrast_data | all(!is.null(upload_expr) & !is.null(type_analysis)) ) ){
    
    upcon = F
    
    if(!is.null(type_analysis)){
      if(type_analysis == "contrast"){
        upcon = T
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

#function to get the background nodes and edges to plot networks
backgroundNET  <- function(selected_nodes, nodes, edges){
  background_nodes =  nodes %>%
    apply(1, function(r, x){
      if( !r["id"] %in% x ){
        # r["color"] = sub(",1)", ",0.03)", r["color"], fixed=T)
        r["color.background"] = sub(",1)", ",0.03)", r["color.background"], fixed=T)
        r["color.border"] = sub(",1)", ",0.03)", r["color.border"], fixed=T)
        }
      return(r)}, selected_nodes) %>% 
    t() %>% 
    data.frame()

background_edges = edges %>%
  apply(1, function(r, x){
    if( !all((r["from"] %in% x), (r["to"] %in% x)) ){
      r["color"] = "rgba(248,248,255,0.9)"};
    return(r)}, selected_nodes) %>% 
  t() %>% 
  data.frame()

return(list(nodes = background_nodes, edges = background_edges))
}

##A function to convert a vector of gene identifiers to symbol
#Accepted identifier_type values are : "ACCNUM","ALIAS", "ENSEMBL",
#"ENSEMBLPROT","ENSEMBLTRANS","ENTREZID","ENZYME","EVIDENCE",
#"EVIDENCEALL","GENENAME","GENETYPE","GO","GOALL","IPI", "MAP","OMIM",
#"ONTOLOGY", "ONTOLOGYALL","PATH", "PFAM", "PMID", "PROSITE",
#"REFSEQ","SYMBOL","UCSCKG","UNIPROT" 
convert_genes_ids <- function(genes, identifier_type)
{
  out <- tryCatch(
    {
      if(sum(is.na(genes)) > 0)
      {
        print("NAs found in gene identifiers, please fix it. Gene returned with their orginal identifiers.")
        return(genes)
      }
      
      mapping <- unlist(AnnotationDbi::mapIds(org.Hs.eg.db::org.Hs.eg.db, genes, "SYMBOL", identifier_type))
      print(paste(length(mapping[is.na(mapping)])," identifiers could not be mapped to gene symbole.", sep =""))
      
      dubs <- mapping[duplicated(mapping)]
      if(!is.null(dubs))
      {
        dubs <- dubs[complete.cases(dubs)]
        mapping[mapping %in% dubs] <- NA
        print(paste(length(dubs)," genes have ambiguous maping to gene symbole and were removed.", sep =""))
      }
      return(mapping)
    },
    error=function(cond)
    {
      print(cond)
      print("No mapping was made because of above error. Genes returned with their original identifiers.")
      return(genes)
    })
  return(out)
}