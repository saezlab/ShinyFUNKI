tabPanel(
  title="COSMOS",
  fluidRow(
    h3("Network visualization"),
    hr(),
    column(12, align="center", 
           sidebarLayout(
             sidebarPanel(
               width = 2,
               uiOutput("select_node_cosmos"),
               checkboxInput("hierarchical_cosmos", label = "Hierarchical layout", value = FALSE),
               downloadButton(
                 outputId = "download_cosmos",
                 label = "Download results"
               )
             ),
             mainPanel(width = 10,
                       visNetwork::visNetworkOutput("network_cosmos"))
           )
    ),
  ),
  hr()
)