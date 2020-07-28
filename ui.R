# UI
source("sub/global.R")
ui = function(request) {
  fluidPage(
    #useShinyjs(),
      title = "DoRothEA",
    
      # Column with title and logo
      fluidRow(
        column(
          12, align="center", 
          img(src="logo_dorothea.png", align = "right", height=75, width=75),
        )),
    
    hr(),
    
    
    fluidRow(
      # select the column to render the plots
      column(
        6, align="center", uiOutput("dorothea_select_contrast")
      ),
      # select TF for specific plots
      column(
        6, align="center", uiOutput("select_tf")
      )
    ),
    
    hr(),
    
    fluidRow(
      #select number of TFs to show in the graphics
      column(
        3, uiOutput("select_top_n_hits")
      ),
      column(
        8, plotOutput("barplot_nes")
      )
    ),
    
    hr(),
     fluidRow(
        column(
          6, plotOutput("tf_bar")
        ),
        column(
          2, uiOutput("dorothea_select_top_n_labels")
        ),
        
        column(
          6, plotOutput("tf_network")
        )
     ),
    hr(),
    
    # Table visualization
      DT::dataTableOutput("dorothea_result"),
    hr(),
    
    # Download content
    fluidRow(
      column(
        6,
        downloadButton("download_dorothea_scores", "Download TF-activities (NES)")
      ),
      column(
        6, 
        downloadButton("download_graphics", "Download figues (.svg)")
      )
    ),
    #bookmarkButton(id = "dorothea_bookmark"),
    hr() 

  ) # close fluidPage
}

