tabPanel(
  title="CARNIVAL", value = "CARNIVAL",
  
  # Visuales previous results --------
  conditionalPanel(
    condition = ("input.an_carnival == 0"),
    uploadResultsObjUI(id = "upload_carnival_results", 
                       label = "Upload CARNIVAL results", 
                       title_bs = "Upload CARNIVAL results", 
                       content = "Visualise the CARNIVAL results that you already have. The format should be a rds file.")
  ),
  
  fluidRow(
    h3("Network visualization"),
    hr(),
    shinyWidgets::dropdown(
      uiOutput("select_node"),
      uiOutput("select_tf_carnival"),
      checkboxInput("hierarchical", label = "Hierarchical layout", value = FALSE),
      uiOutput("down_carnival"),
      downloadObjUI(id = "download_carnival"),
      circle = TRUE,
      status = "primary",
      inputId = "carnival_control",
      icon = icon("sliders"), width = "300px"
    ),
    
    visNetwork::visNetworkOutput("network"),
    
  ),
  hr(),
  fluidRow(
    h3("Pathway Enrichment Analysis"),
    hr(),
    sidebarLayout(
      sidebarPanel(width = 12,
                   fluidRow(
                     column(width = 6, align = "left",
                            awesomeRadio(inputId = "pathEnrich_database",
                                         label = h5("Select database from Omnipath",
                                                    tags$style(type = "text/css", "#q7c_database {vertical-align: top;}"),
                                                    bsButton("q7c_database", label = "", icon = icon("question"), 
                                                             style = "info", size = "extra-small")),
                                         choices = c('SIGNOR', 'NetPath', 'MSigDB', 'SignaLink' = 'SignaLink_pathway', 'Custom'),
                                         selected = 'SIGNOR',
                                         inline = TRUE),
                            bsPopover(id = "q7c_database", 
                                      title = "Select database from Omnipath",
                                      content = "Select source to run a pathway enrichment analysis on CARNIVAL output. If custom option is selected, upload a tsv file containing genes in the first column (HGNC/uniprot ids) and the second the gene sets",
                                      placement = "right", 
                                      trigger = "click", 
                                      options = list(container = "body")
                            ),
                            uiOutput("pathEnrich_msigDB_collection"),
                            uiOutput("pathEnrich_custom"),
                            actionButton("run_PEA", "Run"),
                            hr(),
                            conditionalPanel(
                              condition =  ("input.run_PEA == 1"),
                              uiOutput("down_pea"),
                              downloadObjUI(id = "download_pea"),
                            )
                     ),

                     column(width = 6, align = "left",
                            conditionalPanel(
                              condition = ("input.run_PEA == 1"),
                              fluidRow(
                                column(width = 4,
                                       knobNumericInfoUI(id = "p_value", 
                                                         label = "Adjusted pValue", 
                                                         title_bs = "Adjusted pValue Cutoff",
                                                         content = "Adjusted pValue to use as threshold to show the enriched results.",
                                                         thresholds = list("value"=0.05, "min" = 0, "max" = 1, "step" = 0.01))),
                                column(width = 4,
                                       knobNumericInfoUI(id = "pea_nPaths", 
                                                         label = "Paths/Signatures", 
                                                         title_bs = "Number of Paths/Signatures",
                                                         content = "The number of significant Paths/Signatures to show in plots.",
                                                         thresholds = list("value" = 10, "min" = 5, "max" = 300, "step" = 1))),
                                column(width = 4,
                                       knobNumericInfoUI(id = "pea_nGenes", 
                                                         label = "Genes", 
                                                         title_bs = "Number of Genes",
                                                         content = "The number of significant Genes to show in volcano plot.",
                                                         thresholds = list("value"=10, "min" = 5, "max" = 500, "step" = 1)))
                              )
                            )
                     )
                     
                   )
      ),
      mainPanel(width = 0)
    )
  ),
  fluidRow(
    column(width = 5, plotOutput("barplot_pea")),
    column(width = 6, offset = 1, plotOutput("volcano_pea"))
  ),
  hr(),
  # Table visualization
  DT::dataTableOutput("pea_table"),
  hr()
)