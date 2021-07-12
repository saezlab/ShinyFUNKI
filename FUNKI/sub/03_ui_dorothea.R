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
  
   # fluidRow(
    dropdown(
      # tags$h5("List of Inputs"),
      uiOutput("select_contrast_dorothea"),
      uiOutput("select_tf"),
      uiOutput("select_top_n_hits"),
      uiOutput("select_top_n_labels"),
      uiOutput("down_doro"),
      downloadObjUI(id = "download_dorothea"),
      circle = TRUE, 
      status = "primary",
      inputId = "mydropdown",
      icon = icon("gear"), width = "300px"
    ),
  # ),
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





# conditionalPanel(
#   condition = ("typeof output.dorothea_table !== 'undefined' && output.dorothea_table > 0"),
#   fluidRow(
#     column(6, align = "center",
#            sidebarLayout(
#              sidebarPanel(
#                width = 12,
#                checkboxGroupInput("download_check", 
#                                   h5("Download"), 
#                                   choices = list("DoRothEA scores" = 1, 
#                                                  "Barplot for Sample" = 2, 
#                                                  "Barplot for TF" = 3,
#                                                  "TF's network" = 4,
#                                                  "Heatmap" = 5),
#                                   selected = character(0),
#                                   inline = T),
#                
#                conditionalPanel(condition = ("input.download_check.indexOf('1')>=0"),
#                                   downloadButton(
#                                     "download_dorothea_scores",
#                                     label = "DoRothEA scores"
#                                   )
#                                 ),
#                conditionalPanel(condition = ("input.download_check.indexOf('2')>=0"),#"input.method.indexOf('M')>=0 || input.method.indexOf('S')>=0"
#                                 downloadButton(
#                                   "download_dorothea_barplotSample",
#                                   label = "Barplot for Sample"
#                                 )
#                                 ),
#                conditionalPanel(condition = ("input.download_check.indexOf('3')>=0"),
#                downloadButton("download_dorothea_barplotTF",
#                  label = "Barplot for TF"
#                )),
#                conditionalPanel(condition = ("input.download_check.indexOf('4')>=0"),
#                                 downloadButton("download_dorothea_network",
#                                                label = "TF's network"
#                                 ))
# 
#              ),
#              mainPanel(width = 0)
#            )
#     ),
#   ),
# ),
# hr(),

