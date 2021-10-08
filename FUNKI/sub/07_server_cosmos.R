# Reactive Computations ---------------------------------------------------
cos = uploadResultsObjSever("upload_cosmos_results")

# COSMOS
COSMOS = reactive({
  
  if(input$an_cosmos){
    showModal(modalDialog("Running COSMOS. This may take a while", footer = NULL))
      
      if(input$cosnet == "def"){
        if(any(input$example_data, input$contrast_data)){
          PKN = as.data.frame(read_csv("data/models/cosmos_PKN_example.csv"))
        }
        else{
          PKN = as.data.frame(read_csv("data/models/cosmos_PKN.csv"))
        }
        
      }else{
        PKN = as.data.frame(read_csv(input$upload_cosnet$datapath)) 
      }
      
      if(input$layer1 == 'l1'){
        layer_1 <- as.data.frame(read_csv("data/examples/signaling_input_COSMOS.csv"))
      }else{
        layer_1 <- as.data.frame(read_csv(input$upload_layer1$datapath))
      }
      
      if(input$layer2 == 'l2'){
        layer_2 <- as.data.frame(read_csv("data/examples/metab_input_COSMOS_newPKN.csv"))
      }else{
        layer_2 <- as.data.frame(read_csv(input$upload_layer2$datapath))
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
    removeModal()
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
    dplyr::mutate(color = "grey") %>%
    dplyr::mutate(arrows.to.type = dplyr::if_else(Sign ==1, "arrow", "circle")) %>%
    dplyr::mutate(enabled = TRUE) %>%
    dplyr::mutate(scaleFactor = 1) %>%
    unique.data.frame()
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
    dplyr::mutate(shape = dplyr::case_when(type == "TF" ~ "diamond",
                                           type == "Kinase" ~ "triangle",
                                           type == "protein" ~ "square",
                                           type == "metab_enzyme" ~ "square",
                                           TRUE ~ "circle")) %>%
    dplyr::mutate(title = paste0("<p><b>", label, "</b><br> (", AvgAct, ")</p>")) %>%
    dplyr::mutate(color.background = findInterval(AvgAct, 
                                       seq(from = min(COSMOS()[[2]]$AvgAct), to = max(COSMOS()[[2]]$AvgAct), 
                                           length.out = 100))) %>%
    dplyr::mutate(color.background = pal[color.background]) %>%
    dplyr::mutate(color.background = plotly::toRGB(color.background, alpha = 1)) %>%
    dplyr::mutate(shadow = measured == 1) %>%
    dplyr::mutate(color.border = "rgba(128,128,128,1)")
})

paths_cosmos <- reactive({
  req(COSMOS())
  cosmos = COSMOS()
  names(cosmos) = c("weightedSIF", "nodesAttributes")
  paths = calculate_all_paths(cosmos)
})

# Dynamic widgets / RenderUI ----------------------------------------------

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

output$select_node_cosmos = renderUI({
  req(COSMOS())
  
  choices = COSMOS()[[2]] %>% 
    dplyr::filter(ZeroAct != 1) %>%
    dplyr::select(Nodes) %>%
    dplyr::pull() %>%
    stringr::str_sort(numeric = T) %>%
    unique()
  
  choices = c("select Node", choices)
  
  pickerInput(inputId = "select_node_cosmos",
              label = "Path to Node:",
              choices = choices,
              options = list("live-search" = TRUE),
              selected = "select Node")
})

# Plots ---------------------------------------------------

# network visualisation
output$network_cosmos <- renderVisNetwork({
  
  ## legends
  ledges <- data.frame(color = c("grey", "grey"),
                       label = c("activation", "inhibition"), 
                       arrows.to.type = c("arrow", "circle"),
                       # arrows.type = c("arrow", "circle"),
                       font.align = "top")
  lnodes <- data.frame(label = c("Kinase", "TF", "Protein/Enzyme", "Metabolite"),
                       color = c("gray"),
                       shape = c("triangle", "diamond", "square", "circle"))

  # render network
  visNetwork::visNetwork(nodes_cosmos(), edges_cosmos(), height = 1600, width = 1600) %>%
    visNetwork::visIgraphLayout() %>%
    visNetwork::visEdges(arrows = 'to') %>%
    visNetwork::visLegend(addEdges = ledges, 
                          addNodes = lnodes,
                          width = 0.1, 
                          position = "left", 
                          useGroups = FALSE)
})

observeEvent(input$select_node_cosmos,{
  
  if(input$select_node_cosmos == "select Node"){
    visNetwork::visNetworkProxy("network_cosmos") %>%
      visNetwork::visUpdateNodes(nodes = nodes_cosmos()) %>%
      visNetwork::visUpdateEdges(edges = edges_cosmos())
  }else{
    
    selected_nodes = lapply(paths_cosmos(), function(p, n){
      if( !any(p == n) ){#"ACLY"
       p = NULL 
      }
      return(p)
    }, input$select_node_cosmos)
    
    selected_nodes = do.call(c, selected_nodes) %>% unique()
    
    background = backgroundNET(selected_nodes, nodes_cosmos(), edges_cosmos())
    
    selected_edges = background$edges %>%
      dplyr::filter(grepl(",1)", color, fixed = T)) %>%
      dplyr::pull(id)
    
    message(head(selected_edges))
    
    visNetwork::visNetworkProxy("network_cosmos") %>%
      visNetwork::visUpdateNodes(nodes = background$nodes) %>%
      visNetwork::visUpdateEdges(edges = background$edges) %>%
      visNetwork::visSetSelection(nodesId = selected_nodes,
                                  edgesId = selected_edges) %>%
      visNetwork::visFit(nodes = selected_nodes)
  }
  
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
