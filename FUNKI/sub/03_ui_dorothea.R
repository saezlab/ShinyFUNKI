tabPanel(
  title="DoRothEA", value = "DoRothEA",
  # Row controlling the widgets
  
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_dorothea == 0"),
    fileInput("upload_doro_results", 
              label = h5("Upload DoRothEA results",
                         tags$style(type = "text/css", "#q1_doroVis {vertical-align: top;}"),
                         bsButton("q1_doroVis", label = "", icon = icon("question"), style = "info", size = "extra-small")),
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
    ),
    bsPopover(id = "q1_doroVis", title = "Upload DoRothEA results",
              content = "Visualise the DoRothEA results that you already have. The format should be a csv file with samples in columns and genes in rows",
              placement = "right", 
              trigger = "click", 
              options = list(container = "body")
    )
  ),
  # conditionalPanel(
    # condition = ("output.dorothea_table.length > 0"),
    fluidRow(
      column(6, align = "center",
             sidebarLayout(
               sidebarPanel(
                 width = 12,
                 uiOutput("select_contrast_dorothea"),
                 uiOutput("select_tf"),
                 downloadButton(
                   "download_dorothea_analysis",
                   "Download DoRothEA scores"
                 ),
               ),
               mainPanel(width = 0)
             )
      ),
      
      column(
        6,
        sidebarLayout(
          sidebarPanel(
            width = 12,
            uiOutput("select_top_n_hits"),
            uiOutput("select_top_n_labels")
          ),
          mainPanel(width = 0)
        )
      )
    ),
  # ),
  hr(),
  
  fluidRow(
    column(4,
           # conditionalPanel(
             # condition = ("output.dorothea_table.length = 0 && output.dorothea_table == 'undefined'"),
             # "typeof input.one_rows_selected  !== 'undefined' && input.one_rows_selected.length > 0"
             # condition = ("output.tf_bar !== undefined"),
           #   dropdownButton(
           #     tags$h3("Download"),
           #     # downloadButton("download_dorothea_analysis", "Download DoRothEA scores"),
           #     selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
           #     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
           #     tooltip = tooltipOptions(title = "Click to see inputs !")
           #   )
           # ),
           plotly::plotlyOutput("tf_bar")),
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
