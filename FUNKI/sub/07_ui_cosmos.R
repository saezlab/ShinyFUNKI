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
    shinyWidgets::dropdown(
      label = "Visualisation parameters",
      inputId = "cosmos_control",
      uiOutput("select_node_cosmos"),
      # checkboxInput("hierarchical", label = "Hierarchical layout", value = FALSE),
      uiOutput("down_cosmos"),
      downloadObjUI(id = "download_cosmos"),
      circle = TRUE,
      status = "primary",
      icon = icon("sliders"), width = "300px"
    ),
    hr(),
    visNetwork::visNetworkOutput("network_cosmos"))
)