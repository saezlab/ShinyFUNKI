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
  # Network visualisation --------
  fluidRow(
    h3("Network visualization"),
    hr(),
    shinyWidgets::dropdown(
      label = "Plot parameters",
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
  # Pathway Enrichment Analysis --------
  hr(),
  fluidRow(
    h3("Pathway Enrichment Analysis"),
    hr(),
    sidebarLayout(
      sidebarPanel(width = 12,
                   fluidRow(
                     column(width = 6, align = "left",
                            radioButtons(inputId = "pathEnrich_database",
                                         label = h5("Select resource",
                                                    tags$style(type = "text/css", "#q7c_database {vertical-align: top;}"),
                                                    bsButton("q7c_database", label = "", icon = icon("question"), 
                                                             style = "info", size = "extra-small")),
                                         choices = c('Omnipath', 'Custom'),
                                         selected = character(0),
                                         inline = TRUE),
                            bsPopover(id = "q7c_database", 
                                      title = "Select resource",
                                      content = "Select source to run a pathway enrichment analysis on CARNIVAL output. If custom option is selected, upload a tsv file containing genes in the first column (HGNC/uniprot ids) and the second the gene sets. If Omnipaht is selected, select one of the databases offered.",
                                      placement = "right", 
                                      trigger = "click", 
                                      options = list(container = "body")
                            ),
                            conditionalPanel(
                              condition = ("input.pathEnrich_database == 'Omnipath'"),
                              fluidRow(
                                column(width = 4, uiOutput("select_resource_omnipath")),
                                column(width = 4, uiOutput("set_resource_pea")),
                                column(width = 4, uiOutput("pathEnrich_msigDB_collection"))
                              )
                            ),
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
                              condition = ("input.run_PEA == 0"),
                              DT::dataTableOutput("omnipath_resource"),
                            ),
                            conditionalPanel(
                              condition = ("input.run_PEA == 1"),
                              fluidRow(
                                column(width = 4,
                                       knobInput(
                                         inputId = "p_value",
                                         label = h5("Adjusted pValue",
                                                    tags$style(type = "text/css", paste0("#", "bs_pval_carnival"," {vertical-align: top;}")),
                                                    bsButton("bs_pval_carnival", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                         value = 0.05,
                                         min = 0,
                                         max = 1,
                                         step = 0.01,
                                         displayPrevious = TRUE,
                                         thickness = 0.1,
                                         lineCap = "round",
                                         fgColor = "#428BCA",
                                         inputColor = "#428BCA",
                                         width = "140", height = "140"
                                       ),
                                       bsPopover(id = "bs_pval_carnival",
                                                 title = "Adjusted pValue Cutoff",
                                                 content = "Adjusted pValue to use as threshold to show the enriched results.",
                                                 placement = "right",
                                                 trigger = "click",
                                                 options = list(container = "body")
                                       )
                                ),
                                column(width = 4,
                                       knobInput(
                                         inputId = "pea_nPaths",
                                         label = h5( "Paths/Signatures",
                                                    tags$style(type = "text/css", paste0("#", "bs_nPaths_carnival"," {vertical-align: top;}")),
                                                    bsButton("bs_nPaths_carnival", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                         value = 10,
                                         min = 5,
                                         max = 300,
                                         step = 1,
                                         displayPrevious = TRUE,
                                         thickness = 0.1,
                                         lineCap = "round",
                                         fgColor = "#428BCA",
                                         inputColor = "#428BCA",
                                         width = "140", height = "140"
                                       ),
                                       bsPopover(id = "bs_nPaths_carnival",
                                                 title = "Number of Paths/Signatures",
                                                 content = "The number of significant Paths/Signatures to show in plots.",
                                                 placement = "right",
                                                 trigger = "click",
                                                 options = list(container = "body")
                                       )
                                ),
                                column(width = 4,
                                       knobInput(
                                         inputId = "pea_nGenes",
                                         label = h5( "Genes",
                                                     tags$style(type = "text/css", paste0("#", "bs_nGenes_carnival"," {vertical-align: top;}")),
                                                     bsButton("bs_nGenes_carnival", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                         value = 10,
                                         min = 5,
                                         max = 500,
                                         step = 1,
                                         displayPrevious = TRUE,
                                         thickness = 0.1,
                                         lineCap = "round",
                                         fgColor = "#428BCA",
                                         inputColor = "#428BCA",
                                         width = "140", height = "140"
                                       ),
                                       bsPopover(id = "bs_nGenes_carnival",
                                                 title = "Number of Genes",
                                                 content = "The number of significant Genes to show in volcano plot.",
                                                 placement = "right",
                                                 trigger = "click",
                                                 options = list(container = "body")
                                       )
                                )
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