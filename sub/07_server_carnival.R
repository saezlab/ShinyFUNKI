# Reactive Computations ---------------------------------------------------

output$network <- renderVisNetwork({
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
    dplyr::mutate(color = pal[color])
  
  ## legend for edges
  ledges <- data.frame(color = c("#0578F0", "#F20404", "#777777"),
                       label = c("activation", "inhibition", "involved"), 
                       arrows = c("to", "to", "to"),
                       font.align = "top")
  
  # render network
  visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visLegend(addEdges = ledges, useGroups = T)
})


