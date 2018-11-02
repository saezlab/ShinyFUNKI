tabPanel(
  title="KinAct",
  fluidRow(
    column(
      12, align="center",
      actionButton("run_kinact", label="Run KinAct")
    )
    
  ),
  hr(),
  fluidRow(
    column(
      6, align="center", uiOutput("kinact_select_contrast")
    ),
    column(
      6, align="center", uiOutput("select_kinase")
    )
  ),
  fluidRow(
    column(3),
    column(
      3, uiOutput("kinact_select_top_n_hits")
    ),
    column(
      3,  
      sliderInput(inputId = "kinact_padj_cutoff", 
                       label = "Choose cutoff for adjusted p-value", 
                       min=0.001, max=1, value = 0.05)
    ),
    column(
      3, uiOutput("kinact_select_top_n_labels")
    )
  ),
  hr(),
  fluidRow(
    column(
      4, plotOutput("kinact_lollipop")
    ),
    column(
      4, plotOutput("kinase_volcano")
    ),
    column(
      4, plotOutput("kinact_network")
    )
  ),
  hr(),
  fluidRow(
    column(
      6, plotOutput("kinact_heatmap")
    ),
    column(
      6, plotOutput("kinase_bar")
    )
  ),
  hr(),
  DT::dataTableOutput("kinact_result"),
  hr(),
  fluidRow(
    column(
      6,
      downloadButton("kinact_download_displayed_network", "Download network file (.sif)")
    ),
    column(
      6, 
      downloadButton("download_kinact_scores", "Download Kinase-activities")
    )
  ),
  hr()
)