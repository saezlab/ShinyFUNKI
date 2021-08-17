tabPanel(
  title="KinAct", value = "KinAct",
  # Row controlling the widgets
  
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_kinact == 0"),
    uploadResultsObjUI(id = "upload_kinact_results", 
                       label = "Upload KinAct results", 
                       title_bs = "Upload KinAct results", 
                       content = "Visualise the KinAct results that you already have. The format should be a csv file with samples in columns and genes in rows")
  ),
  
  shinyWidgets::dropdown(
    label = "Visualisation parameters",
    uiOutput("select_contrast_kinact"),
    uiOutput("select_kinase"),
    uiOutput("select_top_kinases"),
    uiOutput("select_top_targets"),
    uiOutput("down_kinact"),
    downloadObjUI(id = "download_kinact"),
    circle = TRUE,
    status = "primary",
    inputId = "kinact_control",
    icon = icon("sliders"), width = "300px"
  ),
  
  fluidRow(
    column(4, plotly::plotlyOutput("kinase_bar")),
    column(4, plotly::plotlyOutput("barplot_nes_kinase")),
    column(4, visNetworkOutput("kinase_network"))
  ),
  
  hr(),
  
  # Row with static heatmap
  fluidRow(plotly::plotlyOutput("heatmap_kinase")),
  
  hr(),
  
  # Table visualization
  DT::dataTableOutput("kinase_table")
)