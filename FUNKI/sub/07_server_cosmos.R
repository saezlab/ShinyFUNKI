# Reactive Computations ---------------------------------------------------

# COSMOS
COSMOS = eventReactive({
  input$an_cosmos
}, {
  
  req(expr(), input$solver)
    
    withProgress(message="Running COSMOS", value=1, {
      
      if(input$cosnet == "def"){
        PKN = as.data.frame(read_csv("data/models/cosmos_PKN.csv"))
      }else{
        PKN = as.data.frame(read_csv(input$upload_cosnet)) 
      }
      
      if(input$layer1 == 'l1'){
        layer_1 <- as.data.frame(read_csv("data/examples/signaling_input_COSMOS.csv"))
      }else{
        layer_1 <- as.data.frame(read_csv(input$upload_layer1))
      }
      
      if(input$layer2 == 'l2'){
        layer_2 <- as.data.frame(read_csv("data/examples/metab_input_COSMOS_newPKN.csv"))
      }else{
        layer_2 <- as.data.frame(read_csv(input$upload_layer2))
      }
      
      # CARNIVAL parameters
      if (input$solver == "lpSolve"){
        solverpath = NULL
      }
      
      cosmos <- run_COSMOS(layer_1 = layer_1, 
                           layer_2 = layer_2, 
                           RNA_data = expr(), 
                           PKN = PKN, 
                           solver = input$solver, 
                           solver_path = solverpath,
                           runtime = c(100,100,100,100))
     
    })
  
})

#objects for visualisation
edges_cosmos <- reactive({
  req(COSMOS())
  # edges and node information for visnetwork
  edges <- COSMOS()$weightedSIF %>%
    dplyr::rename(from = Node1, to = Node2, value = Weight) %>%
    tibble::rowid_to_column(var = "id") %>%
    dplyr::mutate(color = dplyr::case_when(Sign == 1 ~ plotly::toRGB('#0578F0', alpha = 1),
                                           Sign == -1 ~ plotly::toRGB('#F20404', alpha = 1),
                                           Sign == 0 ~ plotly::toRGB('#777777', alpha = 1)))
  
})

nodes_cosmos <- reactive({
  req(COSMOS())
  # create color scale for nodes
  pal_red_blue = c(rev(RColorBrewer::brewer.pal(9, 'Reds')),
                   "#FFFFFF",
                   RColorBrewer::brewer.pal(9, 'Blues'))
  pal = colorRampPalette(pal_red_blue)(100)
  
  #nodes
  nodes <- COSMOS()$nodesAttributes %>%
    dplyr::filter(ZeroAct != 100) %>%
    dplyr::filter(Node %in% c(union(edges_cosmos()$from, edges_cosmos()$to))) %>%
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

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node_cosmos = renderUI({
  req(COSMOS())
    
    choices = COSMOS()$nodesAttributes %>% #C()$nodesAttributes$Node %>%
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
})

# Plots ---------------------------------------------------

# network visualisation
output$network_cosmos <- renderVisNetwork({
  
  ## legends
  ledges <- data.frame(color = c("#0578F0", "#F20404", "#777777"),
                       label = c("activation", "inhibition", "involved"), 
                       arrows = c("to", "to", "to"),
                       font.align = "top")
  
  lnodes <- data.frame(label = c("TF", "Perturbed", "Intermediate"),
                       color = c("gray"),
                       shape = c("triangle", "diamond", "circle"))
  
  # render network
  visNetwork::visNetwork(nodes_cosmos(), edges_cosmos(), height = "500px", width = "100%") %>%
    visNetwork::visIgraphLayout() %>%
    visEdges(arrows = 'to') %>%
    visNetwork::visLegend(addEdges = ledges, addNodes = lnodes,
                          width = 0.1, position = "right", useGroups = FALSE)
})

observeEvent(input$select_node_cosmos,{
  visNetwork::visNetworkProxy("network_cosmos") %>%
    visNetwork::visFocus(id = input$select_node_cosmos, scale = 4)
})

observeEvent(input$hierarchical_cosmos,{
  req(input$hierarchical_cosmos)

    visNetwork::visNetworkProxy("network_cosmos") %>%
      visNetwork::visHierarchicalLayout(levelSeparation = 500,
                                        sortMethod = "directed",
                                        treeSpacing = 20,
                                        edgeMinimization=F, blockShifting=F) %>%
      visNetwork::visPhysics(hierarchicalRepulsion = list(nodeDistance = 300))

})

# Download Handler --------------------------------------------------------

output$download_cosmos = downloadHandler(
  filename = "footprint_cosmos_saezLab.tar.gz",
  content = function(x) {
    fdir = "footprint_cosmos_saezLab"
    
    if (dir.exists(fdir)) {
      do.call(file.remove, list(list.files(fdir, full.names = TRUE)))
    } else{
      dir.create(fdir)
    }
    
    saveRDS(COSMOS(), file = paste0("cosmos_results_", input$select_sample_carnival, ".rds"))
    write.csv(COSMOS()$weightedSIF,
              file.path(fdir, paste0("cosmos_network_", input$select_sample_carnival, ".csv")),
              quote = F)
    write.csv(COSMOS()$nodesAttributes,
              file.path(fdir, paste0("cosmos_nodesAttributes_", input$select_sample_carnival, ".csv")),
              quote = F)
    tar(x, files = fdir, compression = "gzip")
  }
)
