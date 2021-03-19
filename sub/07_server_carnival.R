# Reactive Computations ---------------------------------------------------

C = eventReactive({
  input$run_carnival
  input$solver
  input$progeny
  input$dorothea
  input$net_type
  input$omnipath
  input$inputs_targets
  input$net_complex
  input$upload_network
  input$upload_targets
  input$upload_tfs
  input$upload_progeny
  input$solverPath
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
                   "net_type" = input$net_type)
      }else{net = input$upload_network}
      
      message(input$solverpath)
      run_carnival(data = expr(), 
                   net = net,
                   ini_nodes = input$inputs_targets,
                   dorothea = param_doro, 
                   progeny = param_prog,
                   solver = list(spath = solverpath,
                                 solver = input$solver))
      
    })
  }else{ carnival_result = readRDS("data/examples/carnival_result_celline_SIDM00194.rds") }
  
})

# Dynamic widgets / RenderUI ----------------------------------------------
output$select_node = renderUI({
  if (!is.null(C())) {
    
    choices = C()$nodesAttributes$Node %>%
      stringr::str_sort(numeric = T)
    pickerInput(inputId = "focus_node",
                label = "Focus on node:",
                choices = choices,
                selected = choices[1])
  }
})


# Plots ---------------------------------------------------
output$network <- renderVisNetwork({
   carnival_result = C()
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
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visLegend(addEdges = ledges, addNodes = lnodes,
                          width = 0.1, position = "right", useGroups = FALSE)
})

# observe({
#   message(input$focus_node)
#   visNetwork::visNetworkProxy("network") %>%
#     visNetwork::visFocus(id = input$focus_node, scale = 4)
# })

observe({
  if(input$hierarchical){
    visNetwork::visNetworkProxy("network") %>%
      visNetwork::visHierarchicalLayout(levelSeparation = 500,
                            sortMethod = "directed",
                            treeSpacing = 20,
                            edgeMinimization=F, blockShifting=F) %>%
      visNetwork::visPhysics(hierarchicalRepulsion = list(nodeDistance = 300))
  }
})



