tabPanel(
  title="CARNIVAL",
  fluidRow(
    column(12, align="center", 
      sidebarLayout(
        sidebarPanel(
          width = 12,
          fluidRow(
            column(6, align = "center",
                   h3("Network visualization"),
                   fluidRow(
                     column(6, align = "center", uiOutput("select_node")),
                     column(6, align = "center", uiOutput("select_tf_carnival"))
                   ),
                   checkboxInput("hierarchical", label = "Hierarchical layout", value = FALSE)
                   ),
            column(6, align = "center",
                   h3("Pathway Enritchment Analysis"),
                   awesomeRadio(inputId = "pathEnrich_database",
                                label = h5("Select database from Omnipath",
                                                   tags$style(type = "text/css", "#q7c_database {vertical-align: top;}"),
                                                   bsButton("q7c_database", label = "", icon = icon("question"), 
                                                            style = "info", size = "extra-small")),
                                choices = c('SIGNOR', 'NetPath', 'MSigDB', 'SignaLink', "Custom"),
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
                   actionButton("run_PEA", "Run")
            )
            
          )
        ),
        mainPanel(width = 0)
      )
    ),
  ),
  hr(),
  fluidPage(
    visNetwork::visNetworkOutput("network")
  ),
  hr(),
  fluidRow(align = "center", 
           sidebarLayout(
             sidebarPanel(width = 2,
                          numericInput(inputId = "pea_thresbold",
                                       label = h5("Adjusted pValue",
                                                  tags$style(type = "text/css", "#q7pea_pval {vertical-align: top;}"),
                                                  bsButton("q7pea_pval", label = "", icon = icon("question"),
                                                           style = "info", size = "extra-small")),
                                       min = 0, max = 1, value = 0.05
                          ),
                          bsPopover(id = "q7pea_pval",
                                    title = "Adjusted pValue Cutoff",
                                    content = "Adjusted pValue to use as threshold to show the enriched results.",
                                    placement = "right",
                                    trigger = "click",
                                    options = list(container = "body")
                          ),
                          numericInput(inputId = "pea_nPaths",
                                       label = h5("Number of Paths/Signatures",
                                                  tags$style(type = "text/css", "#q7pea_npaths {vertical-align: top;}"),
                                                  bsButton("q7pea_npaths", label = "", icon = icon("question"), 
                                                           style = "info", size = "extra-small")),
                                       min = 1, max = NA, value = 5
                          ),
                          bsPopover(id = "q7pea_npaths", 
                                    title = "Number of Paths/Signatures",
                                    content = "The number of significant Paths/Signatures to show in plots",
                                    placement = "right", 
                                    trigger = "click", 
                                    options = list(container = "body")
                          ),
                          numericInput(inputId = "pea_nGenes", 
                                       label = h5("Number of Genes",
                                                  tags$style(type = "text/css", "#q7pea_ngenes {vertical-align: top;}"),
                                                  bsButton("q7pea_ngenes", label = "", icon = icon("question"), 
                                                           style = "info", size = "extra-small")),
                                       min = 1, max = NA, value = 4
                          ),
                          bsPopover(id = "q7pea_ngenes", 
                                    title = "Number of Genes",
                                    content = "The number of significant Genes to show in volcano plot",
                                    placement = "right", 
                                    trigger = "click", 
                                    options = list(container = "body")
                          ),
                          downloadButton(
                            outputId = "download_pea_analysis",
                            label = "Download EA/igures"
                          )
             ),
             mainPanel(width = 10,
                       column(width = 4, offset = 1, plotOutput("barplot_pea")),
                       column(width = 6, offset = 1, plotOutput("volcano_pea"))
                       
                       )
           )
  ),
  hr(),
  # Table visualization
  DT::dataTableOutput("pea_table"),
  hr()
)