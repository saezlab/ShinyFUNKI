tabPanel(
  title="CARNIVAL",
  fluidRow(
    column(
      11, align="center", 
      sidebarLayout(
        sidebarPanel(
          width = 12,
          fluidRow(
            column(3,
                   selectInput(inputId = "inputs_targets", label = "Select or Upload Targets",
                               choices = c("All posible from network" = "all_inputs",
                                           "Let CARNIVAL choose them" = NULL, 
                                           "Upload" = "up"),
                               selected = "inverse"),
                   conditionalPanel(
                     condition = "input.inputs_targets == up",
                     fileInput("upload_network", label = NULL))
            ),
            column(3,
                   radioButtons("omnipath", label = "Select or Upload Network", 
                                choices = c("Omnipath" = "omni", "Upload" = "up"), 
                                selected = "omni",
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.omnipath == up",
                     fileInput("upload_network", label = NULL)),
                   radioButtons("net_type", 
                                label = "Type of network", 
                                choices = c("Gene" = "gene", "Protein" = "protein"), 
                                selected = "gene",
                                inline = TRUE)
            ),
            column(3,
                   radioButtons("dorothea", label = "Select or Upload TF's activities", 
                                choices = c("DoRothEA" = "doro", "Upload" = "up"), 
                                selected = "doro",
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.dorothea == false",
                     fileInput("upload_tfs", label = "Upload TF's activities")),
                   actionButton("run_carnival", label="Run CARNIVAL")
            ),
            column(3,
                   radioButtons("progeny", label = "Select or Upload measurments", 
                                choices = c("PROGENy" = "prog", "Upload" = "up"), 
                                selected = "prog",
                                inline = TRUE),
                   conditionalPanel(
                     condition = "input.progeny == false",
                     fileInput("upload_progeny", label = "Upload measurments"))
                   )
          ),
        ),
        mainPanel(width = 0)
      )
    ),
    column(
      1, align="center", 
      img(src="logo_CARNIVAL.png", align = "right", height=75, width=75)
    )
  ),
  hr(),
  fluidPage(
    visNetwork::visNetworkOutput("network")
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
  #bookmarkButton(id = "dorothea_bookmark"),
  hr()
)