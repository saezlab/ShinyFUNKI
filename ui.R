ui = function(request) {
  source("sub/global.R")
  
  fluidPage(
    # Row with title and logos
    titlePanel(fluidRow(
      column(4, "PROGENy"),
      column(
        6,
        offset = 2,
        img(
          src = "logo_saezlab.png",
          align = "right",
          height = 75,
          width = 75
        ),
        img(
          src = "logo_funki.png",
          align = "right",
          height = 75,
          width = 150
        ),
        img(
          src = "logo_progeny.png",
          align = "right",
          height = 75,
          width = 150
        ),
      )
    )),
    
    hr(),
    
    # Row with static heatmap
    fluidRow(plotOutput("heatmap_scores")),
    
    hr(),
    
    # Row controlling the widgets
    sidebarLayout(
      sidebarPanel(
        width = 12,
        uiOutput("select_contrast"),
        uiOutput("select_pathway"),
        downloadButton(
          "download_progeny_analysis",
          "Download PROGENy scores and figures"
        ),
        downloadButton("download_scatter", "Download scatter plot"),
        downloadButton("download_barplot", "Download barplot"),
        downloadButton("download_heatmap", "Download heatmap")
      ),
      mainPanel(width = 0)
    ),
    
    hr(),
    
    # Row showing plots
    fluidRow(column(6, plotOutput("scatter")),
             column(6, plotOutput("barplot_nes"))),
    hr(),
    
    # Table visualization
    DT::dataTableOutput("progeny_result"),
    
    hr()
    
  ) # close fluidPage
}
