# UI


ui = function(request) {
  source("sub/global.R")
  
  fluidPage(
    # Row with title and logos
    titlePanel(fluidRow(
      column(4, "DoRothEA"),
      column(
        4,
        offset = 4,
        img(
          src = "logo_saezlab.png",
          align = "right",
          height = 75,
          width = 75
        ),
        img(
          src = "logo_dorothea.png",
          align = "right",
          height = 75,
          width = 75
        ),
      )
    )),
    
    hr(),
    
    # Row controlling the widgets
    fluidRow(column(
      6,
      sidebarLayout(
        sidebarPanel(
          width = 12,
          uiOutput("select_contrast"),
          uiOutput("select_tf") ,
          downloadButton(
            "download_dorothea_analysis",
            "Download DoRothEA scores and figures"
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
    )),
    
    # column(4,
    #   sidebarLayout(
    #     sidebarPanel( width = 12,
    #       downloadButton("download_dorothea_scores", "Download TF-activities (NES)"),
    #       downloadButton("download_graphics", "Download figues (.svg)")
    #     ),
    #     mainPanel( width = 0 )
    #   )
    # ),
    
    
    hr(),
    
    fluidRow(
      column(3, plotOutput("tf_bar")),
      column(4, plotOutput("barplot_nes")),
      
      column(5, plotOutput("tf_network"))
    ),
    hr(),
    
    # Table visualization
    DT::dataTableOutput("dorothea_result"),
    hr(),
    
    # Download content
    # fluidRow(
    #   column(
    #     6,
    #     downloadButton("download_dorothea_scores", "Download TF-activities (NES)")
    #   ),
    #   column(
    #     6,
    #     downloadButton("download_graphics", "Download figues (.svg)")
    #   )
    # ),
    #bookmarkButton(id = "dorothea_bookmark"),
    hr()
    
  ) # close fluidPage
}
