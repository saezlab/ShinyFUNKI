# Reactive Computations ---------------------------------------------------
cos = uploadResultsObjSever("upload_cosmos_results")

# COSMOS
COSMOS = reactive({
  
  if(input$an_cosmos){
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
      if (input$solver_cosmos == "lpSolve"){
        solverpath_cosmos = NULL
      }

      data = expr() %>%
        tibble::rownames_to_column("HGNC") %>%
        dplyr::select(!HGNC) %>%
        unique.data.frame()
      
      cosmos <- run_COSMOS(layer_1 = layer_1, 
                           layer_2 = layer_2, 
                           RNA_data = data, 
                           PKN = PKN, 
                           solver = input$solver_cosmos, 
                           solver_path = solverpath_cosmos,
                           runtime = c(200,200,1000,1000))
    })
  }else{
    cosmos = cos()
  }
  
  return(cosmos)
})

#objects for visualisation
edges_cosmos <- reactive({
  req(COSMOS())
  # edges and node information for visnetwork
  edges <- COSMOS()[[1]] %>%
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
  nodes <- COSMOS()[[2]] %>%
    dplyr::filter(ZeroAct != 1) %>%
    dplyr::filter(Nodes %in% c(union(edges_cosmos()$from, edges_cosmos()$to))) %>%
    dplyr::rename(id = Nodes) %>%
    dplyr::mutate(label = id) %>%
    dplyr::mutate(shape = dplyr::case_when(NodeType == "T" ~ "triangle",
                                           NodeType == "S" ~ "diamond",
                                           TRUE ~ "circle")) %>%
    dplyr::mutate(title = paste0("<p><b>", label, "</b><br> (", AvgAct, ")</p>")) %>%
    dplyr::mutate(color = findInterval(AvgAct, seq(from = -1, to = 1, length.out = 50))) %>%
    dplyr::mutate(color = pal[color]) %>%
    dplyr::mutate(color = plotly::toRGB(color, alpha = 1))
  
  
})

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node_cosmos = renderUI({
  req(COSMOS())
    
    choices = COSMOS()[[2]] %>%
      dplyr::filter(ZeroAct != 1) %>%
      dplyr::select(Nodes) %>%
      dplyr::pull() %>%
      stringr::str_sort(numeric = T) %>%
      unique()
    
    pickerInput(inputId = "focus_node_cosmos",
                label = "Focus on node:",
                choices = choices,
                options = list("live-search" = TRUE),
                selected = NULL)
})

output$down_cosmos = renderUI({
  req(COSMOS())
  choices = list("COSMOS result (rds)" = 1, 
                 "COSMOS network (csv)" = 2, 
                 "COSMOS attributes (csv)" = 3)#,
  # "Heatmap" = 5)
  pickerInput(inputId = "down_cosmos",
              label = "Select Download",
              choices = choices,
              selected = 1)
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

cosmos_download = observeEvent({
  input$down_cosmos
},{
  req(COSMOS())
  
  if(input$down_cosmos == 1){
    a = list(fname = "COSMOS.rds",
             cont = function(file){COSMOS() %>% saveRDS(., file = file)})
  }else if(input$down_cosmos == 2){
    a = list(fname = function(){paste0("COSMOS_network.csv")},
             cont = function(file){write.csv(COSMOS()[[1]], file, quote = F)})
  }else if(input$down_cosmos == 3){ 
    a = list(fname = function(){paste0("COSMOS_nodesAttributes.csv")},
             cont = function(file){write.csv(COSMOS()[[2]], file, quote = F)})
  }else if(input$down_cosmos == 4){
    a = list(fname = paste0("network_cosmos_targets_", input$select_tf, "_", input$select_contrast, ".png"),
             cont = function(file){
               visSave(network_tf_reactive(), "temp.html")
               webshot::webshot("temp.html", zoom = 2, file = file)
               file.remove("temp.html")})
  }
  
  downloadObjSever("download_cosmos", filename = a$fname, content = a$cont)
})
