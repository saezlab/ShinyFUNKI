tabPanel(
  title = "Data and Parameters",
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        width = 12,
        fluidRow(h3("Upload Data"))
      ),
      mainPanel(width = 0)
    )
  ),
  fluidRow(
    column(4, align="left",
           
           # show the upload stage if the example is not selected
           fluidRow(
             column(7, align = "center",
                    # data upload
                    fileInput("upload_expr", 
                              label = h5("Upload gene expression (.csv)",
                                         tags$style(type = "text/css", "#q2_data {vertical-align: top;}"),
                                         bsButton("q2_data", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")
                    ),
                    
                    bsPopover(id = "q2_data", title = "Upload data",
                              content = "file with comma-separated-values. If there are multiple conditions, samples in columns and gene (HGNC symbol) in rows. If contrasts, HGNC symbol as row names and at least a column called t with the statistic value from the differential expression analysis. For COSMOS analysis, a column called ID with entrez ids should be provided (it the default networks is going to be used).",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
                    
             ),
             column(5, align = "center",
                    # select organism
                    selectInput("select_organism", 
                                label = h5("Select Organism",
                                           tags$style(type = "text/css", "#q2_organism {vertical-align: top;}"),
                                           bsButton("q2_organism", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                choices = c("Homo sapiens" = "Human",
                                            "Mus musculus" = "Mouse"),
                                selected = "Human"),
                    bsPopover(id = "q2_organism", title = "Select Organism",
                              content = "The model organism. Currently available for Human or Mouse.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
                    
             )
           ),
           
           # select example data
           h4("Load Examples"),
           h5("Expression"),
           fluidRow(
             column(6, align = "center",
                    materialSwitch(inputId = "example_data",
                                   label = "Multiple conditions",
                                   value = FALSE,
                                   status = "default",
                                   width = "100%"),
                    p("Dataset taken from ",
                      a("Blackham et al, J Virol., 2010", 
                        href = "https://www.ncbi.nlm.nih.gov/pubmed/20200238",
                        target = "_blank"),
                      a("(GSE20948)",
                        href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE20948",
                        target = "_blank"))
                    
             ),
             column(6, align = "center",
                    materialSwitch(inputId = "contrast_data",
                                   label = "Constrast",
                                   value = FALSE,
                                   status = "default",
                                   width = "100%"),
                    p("Dataset taken from ",
                      a("Dugourd et al, Mol. Sys. Biology, 2021", 
                        href = "https://www.embopress.org/doi/full/10.15252/msb.20209730",
                        target = "_blank")),
                    
             )
           ),
           h5("Phosphoproteomics"),
           materialSwitch(inputId = "phospho_data", 
                          label = "Phosphodata", 
                          value = FALSE,
                          status = "default",
                          width = "100%"),
           p("Dataset taken from ",
             a("Gonçalves et al, Met Eng, 2018", 
               href = "https://pubmed.ncbi.nlm.nih.gov/29191787/",
               target = "_blank"))
    ),
    
    column(8,
           DT::dataTableOutput("expr")  
    )  
  ),
  
  ### ANALYSIS ###
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        width = 12,
        fluidRow(h3("Select analysis and specific parameters"))
      ),
      mainPanel(width = 0)
    )
  ),
  
  # Dorothea ------------------
  fluidRow(
    column(1, align = "center", 
           img(src = "logo_dorothea.png", align = "right", height = 75, width = 75)
    ),
    column(11,
           fluidRow(
             column(3, align="center",
                    awesomeCheckboxGroup(inputId = "selected_conf_level",
                                         label = h5("Select Confidence Level",
                                                    tags$style(type = "text/css", "#q2d_conflev {vertical-align: top;}"),
                                                    bsButton("q2d_conflev", label = "", icon = icon("question"), 
                                                             style = "info", size = "extra-small")),
                                         choices = c("A", "B", "C", "D", "E"),
                                         selected = c("A","B", "C"),
                                         inline = TRUE),
                    bsPopover(id = "q2d_conflev", 
                              title = "Confidence level of DoRothEA regulons",
                              content = "The confidence assigment comprises 5 levels, ranging from A (highest) to E (lowest).",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
             ),
             column(4, align="center",
                    numericInput(inputId = "minsize", 
                                 label = h5("Regulon's minimal size",
                                            tags$style(type = "text/css", "#q2d_regulon {vertical-align: top;}"),
                                            bsButton("q2d_regulon", label = "", icon = icon("question"), 
                                                     style = "info", size = "extra-small")),
                                 min = 1, max = NA, value = 5
                    ),
                    bsPopover(id = "q2d_regulon", 
                              title = "Minimal size of the regulon",
                              content = "Minimun number of genes targeted by a transcription factor.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
             ),
             column(3, align="center",
                    selectInput(inputId = "method", 
                                label = h5("Method for computing signatures",
                                           tags$style(type = "text/css", "#q2d_method {vertical-align: top;}"),
                                           bsButton("q2d_method", label = "", icon = icon("question"), 
                                                    style = "info", size = "extra-small")),
                                choices = c("scale" = "scale",
                                            "rank" = "rank", 
                                            "mad" = "mad",
                                            "ttest" = "ttest", 
                                            "none" = "none"),
                                selected = "none"
                    ),
                    bsPopover(id = "q2d_method", 
                              title = "Method for computing signature",
                              content = "Method for computing the single sample signatures.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
             ),
             column(1, align="center",
                    actionButton("an_dorothea", "Run DoRothEA") )
           )
    )
  ),
  hr(),
  # Progeny ----------------------------
  fluidRow(
    column(1, align="center", 
           img(src="logo_progeny.png", align = "right", height=75, width=120)
    ),
    column(11,
           fluidRow(
             column(5, align="center",
                    numericInput(inputId = "perm", 
                                 label = h5("Number of permutations",
                                            tags$style(type = "text/css", "#q2p_permu {vertical-align: top;}"),
                                            bsButton("q2p_permu", label = "", icon = icon("question"), 
                                                     style = "info", size = "extra-small")),
                                 min = 1, value = 1000),
                    bsPopover(id = "q2p_permu", 
                              title = "Number of permutations",
                              #progeny pathway scores are computed and their significance assessed 
                              content = "Progeny pathway scores are computed, and their significance assessed, using a gene sampling-based permutation strategy.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body"))
             ),
             column(5, align="center",
                    numericInput(inputId = "top", 
                                 label = h5("Top genes for model matrix", 
                                            tags$style(type = "text/css", "#q2p_top {vertical-align: top;}"),
                                            bsButton("q2p_top", label = "", icon = icon("question"), 
                                                     style = "info", size = "extra-small")),
                                 min = 1, value = 100),
                    bsPopover(id = "q2p_top", 
                              title = "Top genes for model matrix",
                              content = "The top n genes for generating the model matrix according to significance (p-value).",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    )
             ),
             column(1, align="center",
                    actionButton("an_progeny", "Run PROGENy")
             )
           )
    )
  ),
  hr(),
  
  # CARNIVAL ------------------------------------
  fluidRow(
    column(1, align="center", 
           img(src="logo_CARNIVAL.png", align = "right", height=75, width=75)
    ),
    ## targets -----------
    column(11,
           fluidRow(
             column(3, align="center",
                    selectInput(inputId = "inputs_targets", 
                                label = h5("Targets",
                                           tags$style(type = "text/css", "#q2c_inputs {vertical-align: top;}"),
                                           bsButton("q2c_inputs", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                choices = c("All from given network" = "all_inputs",
                                            "Let CARNIVAL choose them" = "inverse", 
                                            "My own list of targets" = "up"),
                                selected = "inverse"),
                    bsPopover(id = "q2c_inputs", 
                              title = "Targets",
                              content = " Target(s) of perturbation. If a list of targets is provided (My own list of targets), a comma-separated-value with HGNC symbols ids is required. The option: all from given network, will take all the most distant nodes of the provided network. The option: Let CARNIVAL choose them, runs inverseCARNIVAL, where the most suitable distant nodes will be selected automatically",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    conditionalPanel(
                      condition =  ("input.inputs_targets == 'up'"),
                      fileInput("upload_targets", 
                                label = NULL, accept = ".csv")
                    ),
                    ## sample -----------
                    conditionalPanel(
                      condition = ("input.example_data"),
                      uiOutput("select_sample_carnival",
                               label = h5("Select Sample or Contrast",
                                          tags$style(type = "text/css", "#q2c_sample {vertical-align: top;}"),
                                          bsButton("q2c_sample", label = "", icon = icon("question"), style = "info", size = "extra-small"))),
                      bsPopover(id = "q2c_sample", 
                                title = "Select Sample or Contrast",
                                content = "The selected sample will be used of the CARNIVAL analysis.",
                                placement = "right", 
                                trigger = "click", 
                                options = list(container = "body")
                      ),
                      
                    )
             ),
             ## network -----------
             column(2, align="center",
                    radioButtons("omnipath", 
                                 label = h5("Network", 
                                            tags$style(type = "text/css", "#q2c_network {vertical-align: top;}"),
                                            bsButton("q2c_network", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                 choices = c("Omnipath" = "omni", "Upload" = "up"), 
                                 selected = "omni",
                                 inline = TRUE),
                    bsPopover(id = "q2c_network", 
                              title = "Network",
                              content = "Generate a signed and directed network with HGNC symbols using Omnipath. If a network is upload, a comma-separated file with three columns (Source, Interaction, Target) is required. The file should contain HGNC symbols.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    conditionalPanel(
                      condition = ("input.omnipath == 'up'"),
                      fileInput("upload_network", label = NULL, accept = ".csv")),
                    checkboxInput("net_complex", 
                                  label = "Add complexes",
                                  value = TRUE)
             ),
             ## dorothea -----------
             column(2, align="center",
                    radioButtons("dorothea", 
                                 label = h5("TF's activities", 
                                            tags$style(type = "text/css", "#q2c_doro {vertical-align: top;}"),
                                            bsButton("q2c_doro", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                 choices = c("DoRothEA" = "doro", "Upload" = "up"), 
                                 selected = "doro",
                                 inline = TRUE),
                    bsPopover(id = "q2c_doro", 
                              title = "Activities of transcription factors",
                              content = "Calculate DoRothEA normalised enrichment scores based on the above selected parameters. Alternatively, a file with comma-separeted activites is requiered.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    conditionalPanel(
                      condition = ("input.dorothea == 'up'"),
                      fileInput("upload_tfs", label = NULL, accept = ".csv")),
             ),
             ## progeny -----------
             column(2, align="center",
                    radioButtons("progeny", 
                                 label = h5("Weights", 
                                            tags$style(type = "text/css", "#q2c_proge {vertical-align: top;}"),
                                            bsButton("q2c_proge", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                 choices = c("PROGENy" = "prog", "Upload" = "up"), 
                                 selected = character(0),
                                 inline = TRUE),
                    bsPopover(id = "q2c_proge", 
                              title = "Weights",
                              content = "Calculate PROGENy scores based on the above selected parameters. Alternatively, a file with comma-separeted scores (ranged between -1 and 1) per gene is requiered.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    conditionalPanel(
                      condition = ("input.progeny == 'up'"),
                      fileInput("upload_progeny", label = NULL, accept = ".csv"))
             ),
             ## solver -----------
             column(3, align="center",
                    radioButtons("solver", 
                                 label = h5("Solver", 
                                            tags$style(type = "text/css", "#q2c_solver {vertical-align: top;}"),
                                            bsButton("q2c_solver", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                                 choices = c("lpSolve" = "lpSolve", "cplex" = "cplex", "cbc" = "cbc"), 
                                 selected = "cplex",
                                 inline = TRUE),
                    bsPopover(id = "q2c_solver", 
                              title = "Solver",
                              content = "Select solver to run the optimization. When cbc/cplex is selected, a path to the executable file is requiered.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    bsPopover(id = "q2c_network", 
                              title = "Network",
                              content = "Generate a signed and directed network using Omnipath. If a network is upload, a tab-separated file with three headers (Source, Interaction, Target) is required. The network is based on genes, when it contains HGNC symbols, or proteins, when it contains uniprot ids.",
                              placement = "right", 
                              trigger = "click", 
                              options = list(container = "body")
                    ),
                    conditionalPanel(
                      condition = ("input.solver != 'lpSolve'"),
                      shinyFilesButton(id = 'solverPath', 
                                       label = 'Select path of cbc/cplex file', 
                                       title = NULL, 
                                       multiple = FALSE),
                      # fileInput("solverPath", label = "Select path to execute cbc/cplex file")
                    ),
                    actionButton("an_carnival", "Run CARNIVAL")
             )
             
           )
           
    )
  ),
  # COSMOS ------------------------------------
  hr(),
  fluidRow(
    column(1, align="center",
           img(src="logo_cosmos.png", align = "right", height=75, width=75)
    ),
    column(2, align="center",
           radioButtons("cosnet",
                        label = h5("Network",
                                   tags$style(type = "text/css", "#q1c_cosnet {vertical-align: top;}"),
                                   bsButton("q1c_cosnet", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                        choices = c("Default" = "def", "Upload" = "up"),
                        selected = "def",
                        inline = TRUE),
           bsPopover(id = "q1c_cosnet",
                     title = "Network",
                     content = "Prior knowledge network (PKN).",
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
           ),
           conditionalPanel(
             condition = ("input.cosnet == 'up'"),
             fileInput("upload_cosnet", label = NULL)),
    ),
    column(2, align="center",
           radioButtons(inputId = "layer1",
                        label = h5("Layer 1",
                                   tags$style(type = "text/css", "#q2c_layer1 {vertical-align: top;}"),
                                   bsButton("q2c_layer1", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                        choices = c("Example Phosphoproteomics" = "l1", "Upload" = "up"), 
                        selected = "l1",
                        inline = TRUE),
           bsPopover(id = "q2c_layer1",
                     title = "Activities of Layer 1",
                     content = "Numerical vector, where names are nodes identifiers as in the network and values are from 1, 0, -1. Continuous data will be discretized using the sign function.",
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
           ),
           conditionalPanel(
             condition = ("input.layer1 == 'up'"),
             fileInput("upload_layer1", label = NULL, accept = ".csv"))
    ),
    
    column(2, align="center",
           radioButtons(inputId = "layer2",
                        label = h5("Layer 2",
                                   tags$style(type = "text/css", "#q3c_layer2 {vertical-align: top;}"),
                                   bsButton("q3c_layer2", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                        choices = c("Example Metabolomics" = "l2", "Upload" = "up"), 
                        selected = "l2",
                        inline = TRUE),
           bsPopover(id = "q3c_layer2",
                     title = "Activities of Layer 2",
                     content = "Numerical vector, where names are nodes identifiers as in the network and values are from 1, 0, -1. Continuous data will be discretized using the sign function.",
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
           ),
           conditionalPanel(
             condition = ("input.layer2 == 'up'"),
             fileInput("upload_layer2", label = NULL, accept = ".csv"))
    ),
    column(3, align="center",
           radioButtons("solver_cosmos",
                        label = h5("Solver",
                                   tags$style(type = "text/css", "#q4c_solver {vertical-align: top;}"),
                                   bsButton("q4c_solver", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                        choices = c("lpSolve" = "lpSolve", "cplex" = "cplex", "cbc" = "cbc"),
                        selected = "cplex",
                        inline = TRUE),
           bsPopover(id = "q4c_solver",
                     title = "Solver",
                     content = "Select solver to run the optimization. When cbc/cplex is selected, a path to the executable file is requiered.",
                     placement = "right",
                     trigger = "click",
                     options = list(container = "body")
           ),
           conditionalPanel(
             condition = ("input.solver_cosmos != 'lpSolve'"),
             shinyFilesButton(id = 'solverPath',
                              label = 'Select path of cbc/cplex file',
                              title = NULL,
                              multiple = FALSE),
           ),
    ),
    column(1, align="center", actionButton("an_cosmos", "Run COSMOS")),
  ),
  # KinAct ------------------------------------
  hr(),
  fluidRow(
    column(1, align = "center",
           img(src = "logo_kinact.png", align = "right", height = 75, width = 75)
    ),
    column(11,
           fluidRow(
             column(5, align="center",
                    numericInput(inputId = "minsize",
                                 label = h5("Regulon's minimal size",
                                            tags$style(type = "text/css", "#q2d_regulon {vertical-align: top;}"),
                                            bsButton("q2d_regulon", label = "", icon = icon("question"),
                                                     style = "info", size = "extra-small")),
                                 min = 1, max = NA, value = 5
                    ),
                    bsPopover(id = "q2d_regulon",
                              title = "Minimal size of the regulon",
                              content = "Minimun number of genes targeted by a kinase.",
                              placement = "right",
                              trigger = "click",
                              options = list(container = "body")
                    )
             ),
             column(5, align="center",
                    selectInput(inputId = "method",
                                label = h5("Method for computing signatures",
                                           tags$style(type = "text/css", "#q2d_method {vertical-align: top;}"),
                                           bsButton("q2d_method", label = "", icon = icon("question"),
                                                    style = "info", size = "extra-small")),
                                choices = c("scale" = "scale",
                                            "rank" = "rank",
                                            "mad" = "mad",
                                            "ttest" = "ttest",
                                            "none" = "none"),
                                selected = "none"
                    ),
                    bsPopover(id = "q2d_method",
                              title = "Method for computing signature",
                              content = "Method for computing the single sample signatures.",
                              placement = "right",
                              trigger = "click",
                              options = list(container = "body")
                    )
             ),
             column(1, align="center",
                    actionButton("an_kinact", "Run KinAct") )
           )
    )
  )  
  
)
