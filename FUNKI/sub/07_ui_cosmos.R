tabPanel(
  title = "COSMOS", value = "COSMOS", 
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_cosmos == 0"),
    uploadResultsObjUI(id = "upload_cosmos_results", 
                       label = "Upload COSMOS results", 
                       title_bs = "Upload COSMOS results", 
                       content = "Visualise the COSMOS results that you already have. The format should be a rds file.")
  ),
  # Network visualisation --------
  
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