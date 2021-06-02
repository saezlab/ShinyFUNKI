# Reactive Computations ---------------------------------------------------

# CARNIVAL
C = eventReactive({
  input$an_carnival
}, {

  shiny::req(input$solver)
  
  withProgress(message = "Running CARNIVAL...", value = 1, {
      
  if (input$example_data){
        organism = "Human"
  }else {organism = input$select_organism}
      
      #dorthea
  if(input$dorothea == "doro"){
    data = expr() %>% dplyr::select(!!as.name(input$select_sample_carnival))
        param_doro = list("organism" = organism,
                          "confidence_level" = input$selected_conf_level,
                          "minsize" = input$minsize, 
                          "method" = input$method)
  }else{
    data = NULL
    param_doro = input$upload_tfs$datapath
  }
      
      #progeny
  if(!is.null(input$progeny)){
    if(input$progeny == "prog"){
      param_prog = list("organism" = organism, 
                        "top" = input$top, 
                        "perm" = input$perm)
    }else if(input$progeny == "up"){
      param_prog = input$upload_progeny$datapath
    }
  }else{param_prog = NULL}
      
      #network
  if(input$omnipath == "omni"){
        net = list("net_complex" = input$net_complex, 
                   "net_type" = "gene")#input$net_type)
  }else{net = input$upload_network$datapath}
      #targets
  if( input$inputs_targets == "up"){
        targets = input$upload_targets$datapath
  }else{targets = input$inputs_targets}

      # CARNIVAL parameters
  if (input$solver == "lpSolve"){
        solverpath = NULL
  }
  
  carnival_results = run_carnival( data = data,
                                   net = net,
                                   ini_nodes = targets,
                                   dorothea = param_doro,
                                   progeny = param_prog,
                                   solver = list(spath = solverpath,
                                                 solver = input$solver))
    })
  
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
      pathEnreach(nodeAtt = C()$nodesAttributes, 
                  database = input$pathEnrich_database,
                  select_collection = cll)

  })
  
})

#objects for visualisation
edges <- reactive({
  # req(C())
  # edges and node information for visnetwork
  edges <- C()$weightedSIF %>%
    dplyr::rename(from = Node1, to = Node2, value = Weight) %>%
    tibble::rowid_to_column(var = "id") %>%
    dplyr::mutate(color = dplyr::case_when(Sign == 1 ~ plotly::toRGB('#0578F0', alpha = 1),
                                           Sign == -1 ~ plotly::toRGB('#F20404', alpha = 1),
                                           Sign == 0 ~ plotly::toRGB('#777777', alpha = 1)))
  
})

nodes_carnival <- reactive({
  # req(C())
    # create color scale for nodes
    pal_red_blue = c(rev(RColorBrewer::brewer.pal(9, 'Reds')),
                     "#FFFFFF",
                     RColorBrewer::brewer.pal(9, 'Blues'))
    pal = colorRampPalette(pal_red_blue)(100)
    
    #nodes
    nodes <- C()$nodesAttributes %>%
      dplyr::filter(ZeroAct != 100) %>%
      dplyr::filter(Node %in% c(union(edges()$from, edges()$to))) %>%
      dplyr::rename(id = Node) %>%
      dplyr::mutate(label = id) %>%
      dplyr::mutate(shape = dplyr::case_when(NodeType == "T" ~ "triangle",
                                             NodeType == "S" ~ "diamond",
                                             TRUE ~ "circle")) %>%
      dplyr::mutate(title = paste0("<p><b>", label, "</b><br> (", AvgAct, ")</p>")) %>%
      dplyr::mutate(color = findInterval(AvgAct, seq(from = -100, to = 100, length.out = 100))) %>%
      dplyr::mutate(color = pal[color]) %>%
      dplyr::mutate(color = plotly::toRGB(color, alpha = 1))
    
    
  })

paths <- reactive({
  # req(C())
  paths = calculate_all_paths(C())
})

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node = renderUI({
  
  if (!is.null(C())) {

    choices = C()$nodesAttributes %>%
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

  }
})

output$select_tf_carnival = renderUI({
  # if (!is.null(C())) {

  choices = base::setdiff(C()$weightedSIF$Node2,
                          C()$weightedSIF$Node1) # C()$nodesAttributes$Node %>%
  pickerInput(inputId = "select_tf_carnival",
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
      proteins = C()$nodesAttributes %>% dplyr::filter(ZeroAct != 100) %>% dplyr::select(Node) %>% dplyr::pull(),
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
  
  validate(
    need(!is.null(C()), "No network was found")
  )
  
  # req(C())
    ## legends
    ledges <- data.frame(color = c("#0578F0", "#F20404", "#777777"),
                         label = c("activation", "inhibition", "involved"), 
                         arrows = c("to", "to", "to"),
                         font.align = "top")
    
    lnodes <- data.frame(label = c("TF", "Perturbed", "Intermediate"),
                         color = c("gray"),
                         shape = c("triangle", "diamond", "circle"))
    
    # render network
    visNetwork::visNetwork(nodes_carnival(), edges(), height = "500px", width = "100%") %>%
      visNetwork::visIgraphLayout() %>%
      visEdges(arrows = 'to') %>%
      visNetwork::visLegend(addEdges = ledges, addNodes = lnodes,
                            width = 0.1, position = "right", useGroups = FALSE)

})

observeEvent(input$focus_node,{
  visNetwork::visNetworkProxy("network") %>%
    visNetwork::visFocus(id = input$focus_node, scale = 4)
})

observeEvent(input$select_tf_carnival,{

  pat = paste0("_", input$select_tf_carnival, "$")
  
  selected_nodes = do.call(c,paths()[grep(pat, names(paths()))]) %>% unique()
  background_nodes = nodes_carnival() %>%
    apply(1, function(r, x){
      if( !r["id"] %in% x ){
        r["color"] = sub(",1)", ",0.3)", r["color"], fixed=T)};
      return(r)}, selected_nodes) %>% 
    t() %>% 
    data.frame()
  
  background_edges = edges() %>%
    apply(1, function(r, x){
      if( !(r["from"] %in% x) & !(r["to"] %in% x) ){
        r["color"] = sub(",1)", ",0.3)", r["color"], fixed=T)};
      return(r)}, selected_nodes) %>% 
    t() %>% 
    data.frame()
  
  selected_edges = background_edges %>%
    dplyr::filter(grepl(",1)", color, fixed = T)) %>%
    dplyr::pull(id)

  visNetwork::visNetworkProxy("network") %>%
    visNetwork::visUpdateNodes(nodes = background_nodes) %>%
    visNetwork::visUpdateEdges(edges = background_edges) %>%
    visNetwork::visSetSelection(nodesId = selected_nodes, 
                                edgesId = selected_edges)
    
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
  req(PEA(), input$pea_thresbold)
  # if ( !is.null(PEA()) & input$pea_thresbold != 0) {
    
    p <- PEA()$pea %>%
      barplot_pea(threshold_adjpval = input$pea_thresbold,
                  n_paths = input$pea_nPaths)
  # }
  
})

output$barplot_pea = renderPlot({
  print(barplot_pea_reactive())
})

volcano_pea_reactive = reactive ({
  if ( !is.null(PEA()) ) {
    
    volcano_pea(PEA(), 
                C()$nodesAttributes,
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
  req(PEA())
 # if(!is.null(PEA())){

   pea_result_matrix = DT::datatable(
     PEA()$pea %>%
       data.frame() %>%
       tibble::column_to_rownames(var = "pathway") %>%
       round(digits = 3) %>%
       tibble::rownames_to_column(var = "Pathway/Signature"),
     filter = "top",
     extensions = "Buttons",
     options = list(
       paging = TRUE,
       searching = TRUE,
       fixedColumns = TRUE,
       autoWidth = TRUE,
       ordering = TRUE,
       dom = 'tB',
       buttons = c('csv', 'excel'))
   
   )
   
 # }
  
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
    
    saveRDS(C(), file = paste0("carnival_results_", input$select_sample_carnival, ".rds"))
    write.csv(C()$weightedSIF,
              file.path(fdir, paste0("carnival_network_", input$select_sample_carnival, ".csv")),
              quote = F)
    write.csv(C()$nodesAttributes,
              file.path(fdir, paste0("carnival_nodesAttributes_", input$select_sample_carnival, ".csv")),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)
