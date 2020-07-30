# UI
source("sub/global.R")
ui = function(request) {
  fluidPage(
    
    # Row with title and logos
    titlePanel(
      fluidRow(
        column(4, "PROGENy"),
        column(
          6, offset = 2, 
          img( src = "logo_saezlab.png", align = "right", height = 75, width = 75 ),
          img( src = "logo_progeny.png", align = "right", height = 75, width = 150 ),
        ))
      ),

    hr(),
    
    # Row controlling the widgets
      sidebarLayout(
        sidebarPanel( width = 12,
                      uiOutput("select_contrast"),
                      uiOutput("select_pathway"),
                      downloadButton("download_progeny_analysis", "Download PROGENy scores and figures")
        ),
        mainPanel( width = 0 )
      ),
    
     hr(),
 
     fluidRow(
        column(
          3, plotOutput("scatter")
        ),
        column(
          4, plotOutput("barplot_nes")
        ),
        
        column(
          5, plotOutput("heatmap_scores")
        )
     ),
    hr(),
    
    # Table visualization
      DT::dataTableOutput("progeny_result"),
    
    hr()

  ) # close fluidPage
}

