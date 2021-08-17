# Reactive Computations ---------------------------------------------------
carni = uploadResultsObjSever("upload_carnival_results")

# CARNIVAL
C = reactive({
  
  if(input$an_carnival){
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
    
  } else{
    carnival_results = carni()
  }
})

# Pathway enrichment analysis
PEA = eventReactive({
  input$run_PEA
}, {
  req(input$pathEnrich_database)
  
  if (input$pathEnrich_database == 'Omnipath'){
    database = omnipath_resources()[[input$select_resource_omnipath]]
    if(input$select_resource_omnipath == 'MSigDB'){
      database = database %>%
        dplyr::filter(collection == input$pathEnrich_msigDB_collection) %>%
        dplyr::select(genesymbol, geneset)
    } else if(ncol(database) > 4){
      database = database %>%
        dplyr::select(genesymbol, !!as.name(input$set_resource_pea)) %>%
        as.data.frame()
    }else{
      database = database[,c(2, 4)]
    }
  } else if(input$pathEnrich_database == 'Custom'){
    # req(input$upload_custom)
    validate(need(tools::file_ext(input$upload_custom$datapath) == "tsv", "Please upload a tsv file"))
    database = read_tsv(input$upload_custom$datapath)
    # colnames(database) = c("genesymbol", "pathway")
  }
  
  # GSA
  withProgress(message = "Running Enrichment...", value = 1, {
    
    pathEnreach(nodeAtt = C()$nodesAttributes, 
                database = database)
    
  })
  
})

#objects for visualisation
edges <- reactive({
  req(C())
  # edges and node information for visnetwork
  edges <- C()$weightedSIF %>% 
    dplyr::rename(from = Node1, to = Node2, value = Weight) %>%
    tibble::rowid_to_column(var = "id") %>%
    dplyr::mutate(color = "rgba(128,128,128,1)") %>%
    dplyr::mutate(value = value/100) %>%
    dplyr::mutate(arrows.to.type = dplyr::if_else(Sign == 1, "arrow", "circle")) %>%
    dplyr::mutate(enabled = TRUE) %>%
    dplyr::mutate(scaleFactor = 1) %>%
    unique.data.frame()
  
})

nodes_carnival <- reactive({
  req(C())
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
    dplyr::mutate(color.background = findInterval(AvgAct,
                                                  seq(from = min(C()$nodesAttributes$AvgAct),
                                                      to = max(C()$nodesAttributes$AvgAct),
                                                      length.out = 100))) %>%
    dplyr::mutate(color.background = pal[color.background]) %>%
    dplyr::mutate(color.background = plotly::toRGB(color.background, alpha = 1)) %>%
    dplyr::mutate(color.border = "rgba(128,128,128,1)") %>%
    dplyr::mutate(shadow = NodeType == T)
})

paths <- reactive({
  req(C())
  paths = calculate_all_paths(C())
})

omnipath_resources <- reactive({
  req(C())
  if(input$pathEnrich_database == 'Omnipath'){
    OmnipathR::import_omnipath_annotations(proteins = C()$nodesAttribute$Node, wide = TRUE) 
  }
})

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node = renderUI({
  req(C())
  
  choices = C()$nodesAttributes %>% 
    dplyr::filter(ZeroAct != 100) %>%
    dplyr::select(Node) %>%
    dplyr::pull() %>%
    stringr::str_sort(numeric = T) %>%
    unique()
  
  choices = c("select Node", choices)
  
  pickerInput(inputId = "focus_node",
              label = "Focus on node:",
              choices = choices,
              options = list("live-search" = TRUE),
              selected = "select Node")
})

output$select_resource_omnipath = renderUI({
  req(omnipath_resources())
  
  choices = names(omnipath_resources())
  
  pickerInput(inputId = "select_resource_omnipath",
              label = NULL,
              choices = choices,
              options = list("live-search" = TRUE),
              selected = "SIGNOR")
})

output$set_resource_pea = renderUI({
  req(omnipath_resources(),
      input$select_resource_omnipath)
  
  columns_names = names(omnipath_resources()[[input$select_resource_omnipath]])
  
  if(length(columns_names) > 4){
    choices = columns_names[4:length(columns_names)]
    pickerInput(inputId = "set_resource_pea",
                label = NULL,
                choices = choices,
                selected = NULL)
  }
})

output$pathEnrich_msigDB_collection = renderUI({
  shiny::req(omnipath_resources(),
             input$select_resource_omnipath)
  if(input$select_resource_omnipath == 'MSigDB'){
    choices = omnipath_resources()[[input$select_resource_omnipath]] %>%
      dplyr::select(collection) %>% 
      dplyr::pull() %>%
      unique()
    
    pickerInput(inputId = "pathEnrich_msigDB_collection",
                label = NULL,
                choices = choices)
    
  }
  
})

output$pathEnrich_custom = renderUI({
  if (input$pathEnrich_database == 'Custom') {
    
    fileInput("upload_custom", label = NULL, accept = ".tsv")
    
  }
})

output$down_carnival = renderUI({
  req(C())
  choices = list("CARNIVAL result (rds)" = 1, 
                 "CARNIVAL network (csv)" = 2, 
                 "CARNIVAL attributes (csv)" = 3)#,
  # "Heatmap" = 5)
  pickerInput(inputId = "down_carnival",
              label = "Select Download",
              choices = choices,
              selected = 1)
})

output$down_pea = renderUI({
  req(PEA())
  choices = list("PEA results" = 1, 
                 "Volcanoplot" = 2, 
                 "Barplot" = 3)
  pickerInput(inputId = "down_pea",
              label = "Select Download",
              choices = choices,
              selected = 1)
})

# Plots ---------------------------------------------------

# network visualisation
output$network <- renderVisNetwork({
  
  ## legends
  ledges <- data.frame(color = "grey",
                       label = c("activation", "inhibition"), 
                       arrows.to.type = c("arrow", "circle"),
                       font.align = "top")
  
  lnodes <- data.frame(label = c("TF", "Perturbed", "Intermediate"),
                       color = c("grey"),
                       shape = c("triangle", "diamond", "circle"))
  
  # render network
  visNetwork::visNetwork(nodes_carnival(), edges(), height = "500px", width = "100%") %>%
    visNetwork::visIgraphLayout() %>%
    visNetwork::visEdges(arrows = 'to') %>%
    visNetwork::visLegend(addEdges = ledges, 
                          addNodes = lnodes,
                          width = 0.1, 
                          position = "left", 
                          useGroups = FALSE)
  
})

observeEvent(input$focus_node,{
  if(input$focus_node == "select Node"){
    visNetwork::visNetworkProxy("network") %>%
      visNetwork::visUpdateNodes(nodes = nodes_carnival()) %>%
      visNetwork::visUpdateEdges(edges = edges())
  }else{
    selected_nodes = lapply(paths(), function(p, n){
      if( !any(p == n) ){
        p = NULL 
      }
      return(p)
    }, input$focus_node)
    
    selected_nodes = do.call(c, selected_nodes) %>% unique()
    
    background = backgroundNET(selected_nodes, nodes_carnival(), edges())
    
    selected_edges = background$edges %>%
      dplyr::filter(grepl(",1)", color, fixed = T)) %>%
      dplyr::pull(id)
    
    message(head(selected_edges))
    
    visNetwork::visNetworkProxy("network") %>%
      visNetwork::visUpdateNodes(nodes = background$nodes) %>%
      visNetwork::visUpdateEdges(edges = background$edges) %>%
      visNetwork::visSetSelection(nodesId = selected_nodes,
                                  edgesId = selected_edges) %>%
      visNetwork::visFit(nodes = selected_nodes)
  }

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

# enritchment analysis ------------------------------

barplot_pea_reactive = reactive ({
  req(PEA())
  
  validate(
    need((min(PEA()$pea$`Adjusted p-value`) < input$p_value), 
         paste0("The selected Adjusted pValue is lower than the min value: ", round(min(PEA()$pea$`Adjusted p-value`), 2)))
  )
  
  PEA()$pea %>%
    barplot_pea(threshold_adjpval = input$p_value,
                n_paths = input$pea_nPaths)
})

output$barplot_pea = renderPlot({
  print(barplot_pea_reactive())
})

volcano_pea_reactive = reactive ({
  req(PEA())
  
  validate(
    need((min(PEA()$pea$`Adjusted p-value`) < input$p_value), 
         paste0("The selected Adjusted pValue is lower than the min value: ", round(min(PEA()$pea$`Adjusted p-value`), 2)))
  )
  
  volcano_pea(PEA(), 
              C()$nodesAttributes,
              threshold_adjpval = input$p_value,
              n_paths = input$pea_nPaths,
              n_genes = input$pea_nGenes)
  
})

output$volcano_pea = renderPlot({
  print(volcano_pea_reactive())
})

# Render Tables -----------------------------------------------------------
output$omnipath_resource = DT::renderDataTable({
  shiny::req(omnipath_resources(),
             input$select_resource_omnipath)
  columns_names = names(omnipath_resources()[[input$select_resource_omnipath]])
  if( length(columns_names) > 4 ){
    df = omnipath_resources()[[input$select_resource_omnipath]]
    df = df[1:3,4:ncol(df)]
    DT::datatable(df)
  }
})

output$pea_table = DT::renderDataTable({
  
  req(PEA())
  
  pea_result_matrix = DT::datatable(
    PEA()$pea %>%
      data.frame() %>%
      tibble::column_to_rownames(var = colnames(PEA()$annot)[2]) %>%
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
  
})

# Download Handler --------------------------------------------------------
carnival_download = observeEvent({
  input$down_carnival
},{
  req(C())
  
  if(is.null(input$select_sample_carnival)){
    smpl = ""
  }else{
    smpl = paste0("_", input$select_sample_carnival)
  }
  if(input$down_carnival == 1){
    a = list(fname = "CARNIVAL.rds",
             cont = function(file){C() %>% saveRDS(., file = file)})
  }else if(input$down_carnival == 2){
    a = list(fname = function(){paste0("carnival_network", smpl, ".csv")},
             cont = function(file){write.csv(C()$weightedSIF, file, quote = F)})
  }else if(input$down_carnival == 3){ 
    a = list(fname = function(){paste0("carnival_nodesAttributes", smpl, ".csv")},
             cont = function(file){write.csv(C()$weightedSIF, file, quote = F)})
  }else if(input$down_carnival == 4){
    a = list(fname = paste0("network_carnival_targets_", input$select_tf, "_", input$select_contrast, ".png"),
             cont = function(file){
               visSave(network_tf_reactive(), "temp.html")
               webshot::webshot("temp.html", zoom = 2, file = file)
               file.remove("temp.html")})
  }
  
  downloadObjSever("download_carnival", filename = a$fname, content = a$cont)
})

pea_download = observeEvent({
  input$down_pea
},{
  req(PEA())
  
  if(is.null(input$select_sample_carnival)){
    smpl = ""
  }else{
    smpl = paste0("_", input$select_sample_carnival)
  }
  if(input$down_pea == 1){
    a = list(fname = paste0("enrichment_analysis_", input$select_resource_omnipath, smpl, ".csv"),
             cont = function(file){write.csv(PEA()$psa, file = file,  quote = F)})
  }else if(input$down_pea == 2){
    a = list(fname = function(){paste0("volcano_carnivalEA_", input$p_value, smpl, ".png")},
             cont = function(file){ggsave(file, volcano_pea_reactive(), device = "png")})
  }else if(input$down_pea == 3){ 
    a = list(fname = function(){paste0("barplot_carnivalEA_apval", input$p_value, smpl, ".png")},
             cont = function(file){ggsave(file, barplot_pea_reactive(), device = "png")})
  }
  
  downloadObjSever("download_pea", filename = a$fname, content = a$cont)
})