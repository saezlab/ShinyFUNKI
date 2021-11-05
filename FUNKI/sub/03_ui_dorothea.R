tabPanel(
  title="DoRothEA", value = "DoRothEA",
  # Row controlling the widgets
  
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_dorothea == 0"),
    uploadResultsObjUI(id = "upload_dorothea_results", 
                       label = "Upload DoRothEA results", 
                       title_bs = "Upload DoRothEA results", 
                       content = "Visualise the DoRothEA results that you already have. The format should be a csv file with samples in columns and genes in rows")
  ),
  
  shinyWidgets::dropdown(
    label = "Visualisation parameters",
    uiOutput("select_contrast_dorothea"),
    uiOutput("select_tf"),
    uiOutput("select_top_n_hits"),
    uiOutput("select_top_n_labels"),
    uiOutput("down_doro"),
    downloadObjUI(id = "download_dorothea"),
    downloadReportUI(id = "dorothea_report"),
    circle = TRUE,
    status = "primary",
    inputId = "dorotea_control",
    icon = icon("sliders"), width = "300px"
  ),
  
  fluidRow(
    column(4, plotly::plotlyOutput("tf_bar")),
    column(4, plotly::plotlyOutput("barplot_nes_dorothea")),
    column(4, visNetworkOutput("tf_network"))
  ),
  
  hr(),
  
  # Row with static heatmap
  fluidRow(plotly::plotlyOutput("heatmap_dorothea")),
  
  hr(),
  
  # Table visualization
  DT::dataTableOutput("dorothea_table")
)
