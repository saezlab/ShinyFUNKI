# Reactive Computations ---------------------------------------------------

# CARNIVAL
C = eventReactive({
  input$run_carnival
}, {
  
  if (!is.null(input$solver)) {
    
    withProgress(message="Running CARNIVAL", value=1, {
      
      if (input$example_data){
        organism = "Human"
      }else {organism = input$select_organism}
      
      #dorthea
      if(input$dorothea == "doro"){
        param_doro = list("organism" = organism,
                          "confidence_level" = input$selected_conf_level,
                          "minsize" = input$minsize, 
                          "method" = input$method)
      }else{param_doro = input$upload_tfs}
      
      #progeny
      if(input$progeny == "prog"){
        param_prog = list("organism" = organism, 
                          "top" = input$top, 
                          "perm" = input$perm)
      }else{param_prog = input$upload_progeny}
      
      #network
      if(input$omnipath == "omni"){
        net = list("net_complex" = input$net_complex, 
                   "net_type" = "gene")#input$net_type)
      }else{net = input$upload_network}
      
      carnival_results = future_promise({
        run_carnival(data = expr(),
                     net = net,
                     ini_nodes = input$inputs_targets,
                     dorothea = param_doro,
                     progeny = param_prog,
                     solver = list(spath = solverpath,
                                   solver = input$solver))
        })
    })
  }
  
})

# Pathway enrichment analysis
PEA = eventReactive({
  input$run_PEA
}, {

    if (input$pathEnrich_database == 'MSigDB'){
      cll = input$pathEnrich_collection
    }
    else if(input$pathEnrich_database == 'Custom'){
      req(input$upload_custom)
      validate(need(tools::file_ext(input$upload_custom$datapath) == "tsv", "Please upload a tsv file"))
      cll = input$upload_custom$datapath}
    else{ cll = NULL}
    
    # GSA
    withProgress(message = "Running Enrichment...", value = 1, {
      message(input$upload_custom)
      pathEnreach(nodeAtt = carnival_result$nodesAttributes, 
                  database = input$pathEnrich_database,
                  collection = cll)

  })
  
})

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node = renderUI({
  # if (!is.null(C())) {

    choices = carnival_result$nodesAttributes %>% #C()$nodesAttributes$Node %>%
      dplyr::filter(ZeroAct != 100) %>%
      dplyr::select(Node) %>%
      dplyr::pull() %>%
      stringr::str_sort(numeric = T) %>%
      unique()

    pickerInput(inputId = "focus_node",
                label = "Focus on node:",
                choices = choices,
                options = list("live-search" = TRUE),
                selected = NULL)

  # }
})

output$select_tf_carnival = renderUI({
  # if (!is.null(C())) {

  choices = base::setdiff(carnival_result$weightedSIF$Node2,
                          carnival_result$weightedSIF$Node1) #C()$nodesAttributes$Node %>%
  pickerInput(inputId = "focus_tf",
              label = "Path to TF:",
              choices = choices,
              options = list("live-search" = TRUE),
              selected = NULL)
  # }
})

output$pathEnrich_msigDB_collection = renderUI({
  if (input$pathEnrich_database == 'MSigDB') {

    choices = OmnipathR::import_omnipath_annotations(
      resources = 'MSigDB',
      proteins = carnival_result$nodesAttributes %>% dplyr::filter(ZeroAct != 100) %>% dplyr::select(Node) %>% dplyr::pull(),
      wide = TRUE) %>%
      dplyr::select(collection) %>% 
      dplyr::pull() %>%
      unique()
      
      pickerInput(inputId = "pathEnrich_collection",
                label = "Select Specific collection from MSigDB",
                choices = choices)
  }
})

output$pathEnrich_custom = renderUI({
  if (input$pathEnrich_database == 'Custom') {
    
    fileInput("upload_custom", label = NULL, accept = ".tsv")

  }
})

# Plots ---------------------------------------------------

# network visualisation
output$network <- renderVisNetwork({
  # carnival_result = C()
  # create color scale for nodes
  
  pal_red_blue = c(rev(RColorBrewer::brewer.pal(9, 'Reds')), 
                   "#FFFFFF", 
                   RColorBrewer::brewer.pal(9, 'Blues'))
  pal = colorRampPalette(pal_red_blue)(100)
  
  # edges and node information for visnetwork
  edges <- carnival_result$weightedSIF %>%
    dplyr::rename(from = Node1, to = Node2, value = Weight) %>%
    dplyr::mutate(color = dplyr::case_when(Sign == 1 ~ '#0578F0',
                                           Sign == -1 ~ '#F20404',
                                           Sign == 0 ~ '#777777'))
  
  intermediate_nodes = carnival_result$nodesAttributes %>%
    dplyr::filter(NodeType == "" & ZeroAct != 100) %>%
    dplyr::filter(Node %in% c(union(edges$from, edges$to))) %>%
    nrow()

  nodes <- carnival_result$nodesAttributes %>%
    dplyr::filter(ZeroAct != 100) %>%
    dplyr::filter(Node %in% c(union(edges$from, edges$to))) %>%
    dplyr::rename(id = Node) %>%
    dplyr::mutate(label = id) %>%
    dplyr::mutate(shape = dplyr::case_when(NodeType == "T" ~ "triangle",
                                           NodeType == "S" ~ "diamond",
                                           TRUE ~ "ellipse")) %>%
    dplyr::mutate(title = paste0("<p><b>", label, "</b><br> (", AvgAct, ")</p>")) %>%
    dplyr::mutate(color = findInterval(AvgAct, seq(from = -100, to = 100, length.out = 100))) %>%
    dplyr::mutate(color = pal[color]) # %>%
    # dplyr::mutate(level = dplyr::case_when(NodeType == "S" ~ 1,
    #                                        NodeType == "T" ~ 10,
    #                                        NodeType == "" ~ floor(runif( nodes %>% nrow(), 
    #                                                           min = 2, max = 9))))
  
  ## legends
  ledges <- data.frame(color = c("#0578F0", "#F20404", "#777777"),
                       label = c("activation", "inhibition", "involved"), 
                       arrows = c("to", "to", "to"),
                       font.align = "top")
  
  lnodes <- data.frame(label = c("TF", "Perturbed", "Intermediate"),
                       color = c("gray"),
                       shape = c("triangle", "diamond", "ellipse"))
  
  
  # render network
  visNetwork::visNetwork(nodes, edges, height = "500px", width = "100%") %>%
    visNetwork::visIgraphLayout() %>%
    visEdges(arrows = 'to') %>%
    visNetwork::visLegend(addEdges = ledges, addNodes = lnodes,
                          width = 0.1, position = "right", useGroups = FALSE)
})

observeEvent(input$focus_node,{
  visNetwork::visNetworkProxy("network") %>%
    visNetwork::visFocus(id = input$focus_node, scale = 4)
})

observeEvent(input$focus_tf,{
  visNetwork::visNetworkProxy("network") %>%
    visNetwork::visFit(nodes = c("ANGPT4", "RANBP17", "RGS1"))
})

observeEvent(input$hierarchical,{
  if(input$hierarchical){
    visNetwork::visNetworkProxy("network") %>%
      visNetwork::visHierarchicalLayout(levelSeparation = 500,
                                        sortMethod = "directed",
                                        treeSpacing = 20,
                                        edgeMinimization=F, blockShifting=F) %>%
      visNetwork::visPhysics(hierarchicalRepulsion = list(nodeDistance = 300))
  }
})

# enritchment analysis

barplot_pea_reactive = reactive ({
  if ( !is.null(PEA()) ) {
    
    p <- PEA()$pea %>%
      barplot_pea(threshold_adjpval = input$pea_thresbold,
                  n_paths = input$pea_nPaths)
  }
  
})

output$barplot_pea = renderPlot({
  print(barplot_pea_reactive())
})

volcano_pea_reactive = reactive ({
  if ( !is.null(PEA()) ) {
    
    volcano_pea(PEA(), 
                carnival_result$nodesAttributes,
                threshold_adjpval = input$pea_thresbold,
                n_paths = input$pea_nPaths,
                n_genes = input$pea_nGenes)
    
  }
  
})

output$volcano_pea = renderPlot({
  print(volcano_pea_reactive())
})

# Render Tables -----------------------------------------------------------
output$pea_table = DT::renderDataTable({
 if(!is.null(PEA())){

   pea_result_matrix = DT::datatable(
     PEA()$pea %>%
       data.frame() %>%
       tibble::column_to_rownames(var = "pathway") %>%
       round(digits = 3) %>%
       tibble::rownames_to_column(var = "Pathway/Signature"),
     option = list(scrollX = TRUE, autoWidth = T),
     filter = "top"
   )
   
 }
  
})

# Download Handler --------------------------------------------------------

output$download_pea_analysis = downloadHandler(
  filename = "footprint_carnival_EnrichmentAnalysis_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_carnival_EnrichmentAnalysis_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    fnames = c(
      paste0("barplot_carnivalEA_apval", input$pea_thresbold, ".png"),
      paste0("volcano_carnivalEA_", input$pea_thresbold, ".png")
    )
    
    ggsave(file.path(fdir, fnames[1]), barplot_pea_reactive(), device = "png")
    ggsave(file.path(fdir, fnames[2]), volcano_pea_reactive(), device = "png")
    write.csv(PEA()$psa,
              file.path(fdir, paste0("carnivalEA_", input$pathEnrich_database, ".csv")),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)

output$download_carnival = downloadHandler(
  filename = "footprint_carnival_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_carnival_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    saveRDS(carnival_result, file = paste0("carnival_results_", input$select_sample_carnival, ".rds"))
    write.csv(carnival_result$weightedSIF,
              file.path(fdir, paste0("carnival_network_", input$select_sample_carnival, ".csv")),
              quote = F)
    write.csv(carnival_result$nodesAttributes,
              file.path(fdir, paste0("carnival_nodesAttributes_", input$select_sample_carnival, ".csv")),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)





