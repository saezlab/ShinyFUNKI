tabPanel(
  title="DoRothEA",
  fluidRow(
    column(
      12, align="center", 
      img(src="logo_dorothea.png", align = "right", height=75, width=75),
      awesomeRadio(inputId = "quality_vs_coverage", 
                   label="Desired Network property",
                   choices = c("Quality", "Coverage"), 
                   selected = "Quality", inline = T),
      actionButton("run_dorothea", label="Run DoRothEA")
    )
    
  ),
  hr(),
  fluidRow(
    column(
      6, align="center", uiOutput("dorothea_select_contrast")
    ),
    column(
      6, align="center", uiOutput("select_tf")
    )
  ),
  fluidRow(
    column(
      3, awesomeCheckboxGroup(inputId = "selected_conf_level",
                              label = "Select Confidence Level",
                              choices = c("A", "B", "C", "D", "E"),
                              selected = c("A","B"),
                              inline = TRUE)
    ),
    column(
      3, uiOutput("select_top_n_hits")
    ),
    column(
      3,   sliderInput(inputId = "dorothea_padj_cutoff", 
                       label = "Choose cutoff for adjusted p-value", 
                       min=0.001, max=1, value = 0.05)
    ),
    column(
      3, uiOutput("dorothea_select_top_n_labels")
    )
  ),
  hr(),
  fluidRow(
    column(
      4, plotOutput("dorothea_lollipop")
    ),
    column(
      4, plotOutput("tf_volcano")
    ),
    column(
      4, plotOutput("tf_network")
    )
  ),
  hr(),
  fluidRow(
    column(
      6, plotOutput("dorothea_heatmap")
    ),
    column(
      6, plotOutput("tf_bar")
    )
  ),
  hr(),
  DT::dataTableOutput("dorothea_result"),
  hr(),
  fluidRow(
    column(
      6,
      downloadButton("download_dorothea_scores", "Download TF-activities")
    ),
    column(
      6, 
      downloadButton("download_network", "Download network file (.sif)")
    )
  ),
  hr()
)