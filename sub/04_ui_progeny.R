tabPanel(
  title="PROGENy",
  # img(src="logo_progeny.png", align = "right", width = (16/9)*75, height=75),
  # Row controlling the widgets
  sidebarLayout(
    sidebarPanel(
      width = 12,
      fluidRow(align = "center",
               column(6, aling = "center",
                      uiOutput("select_contrast_progeny")
               ),
               column(6, aling = "center",
                      uiOutput("select_pathway")
               )
      ),
      fluidRow(align = "center",
        downloadButton("download_progeny_analysis", "Download PROGENy scores and figures"),
        downloadButton("download_scatter", "Download scatter plot"),
        downloadButton("download_barplot", "Download barplot"),
        downloadButton("download_heatmap", "Download heatmap")
      )
    ),
    mainPanel(width = 0)
  ),
  
  hr(),
  
  # Row showing plots
  fluidRow(column(6, plotOutput("scatter")),
           column(6, plotOutput("barplot_progeny"))
  ),
  
  hr(),
  
  # Row with static heatmap
  fluidRow(plotOutput("heatmap_scores")),
  
  hr(),
  
  # Table visualization
  DT::dataTableOutput("progeny_table"),
  
  hr()
  
)