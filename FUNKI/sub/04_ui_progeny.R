tabPanel(
  title="PROGENy", value = "PROGENy",
  
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_progeny == 0"),
    uploadResultsObjUI(id = "upload_progeny_results", 
                       label = "Upload PROGENy results", 
                       title_bs = "Upload PROGENy results", 
                       content = "Visualise the PROGENy results that you already have. The format should be a csv file with samples in columns and genes in rows")
  ),
  shinyWidgets::dropdown(
    label = "Visualisation parameters",
    uiOutput("select_contrast_progeny"),
    uiOutput("select_pathway"),
    uiOutput("down_progeny"),
    downloadObjUI(id = "download_progeny"),
    downloadReportUI(id = "progeny_report"),
    circle = TRUE,
    status = "primary",
    icon = icon("sliders"), width = "300px",
    inputId = "progeny_control"
    ),

  # Row showing plots
  fluidRow(column(6, plotOutput("scatter")),
           column(6, plotly::plotlyOutput("barplot_progeny"))
  ),
  
  hr(),
  
  # Row with static heatmap
  fluidRow(plotly::plotlyOutput("heatmap_progeny")),
  
  hr(),
  
  # Table visualization
  DT::dataTableOutput("progeny_table")
  
)