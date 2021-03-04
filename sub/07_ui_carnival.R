tabPanel(
  title="CARNIVAL",
  fluidRow(
    column(
      11, align="center", 
      sidebarLayout(
        sidebarPanel(
          width = 12,
          fluidRow(
            
            selectInput("Focus", "Focus on node :",carnival_result$nodesAttributes$Node, selected = NULL),
            checkboxInput("hierarchical", label = "Hierarchical layout", value = FALSE),
            selectInput("function", "Focus on node :",carnival_result$nodesAttributes$Node, selected = NULL),
            
          )
        ),
        mainPanel(width = 0)
      )
    ),
  ),
  hr(),
  fluidPage(
    visNetwork::visNetworkOutput("network")
  ),
  #bookmarkButton(id = "dorothea_bookmark"),
  hr()
)