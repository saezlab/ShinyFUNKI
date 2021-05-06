tabPanel(
  title="KinAct", value = "KinAct",
  fluidRow(
    
    column(6, align = "center",
           sidebarLayout(
             sidebarPanel(
               width = 12,
               uiOutput("select_contrast_kinact"),
               uiOutput("select_kinase"),
               downloadButton(
                 "download_kinact_analysis",
                 "Download Kinact scores and figures"
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
          uiOutput("select_top_kinases"),
          uiOutput("select_top_targets")
        ),
        mainPanel(width = 0)
      )
    )
  ),
  hr(),

  fluidRow(
    column(4, plotly::plotlyOutput("kinase_bar")),
    column(4, plotly::plotlyOutput("barplot_nes_kinase")),
    column(4, plotOutput("kinase_network"))
  ),

  hr(),

  # Row with static heatmap
  fluidRow(plotly::plotlyOutput("heatmap_kinase")),

  hr(),

  # Table visualization
  DT::dataTableOutput("kinase_table")

)