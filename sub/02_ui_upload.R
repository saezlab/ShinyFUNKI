tabPanel(
  title = "Data and Parameters",
  fluidRow(
    h3("Upload Data"),
    column(4, align="center",
           
      #load the example data by default
      p("Example dataset taken from ",
        a("Blackham et al, J Virol., 2010", 
          href = "https://www.ncbi.nlm.nih.gov/pubmed/20200238",
          target = "_blank"),
        a("(GSE20948)",
          href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE20948",
          target = "_blank")),
      switchInput(inputId = "example_data", label = "Example data",
                  onLabel = "Yes", offLabel = "No", value=TRUE),

      #show the upload stage if the example is not selected
      conditionalPanel(
        condition = ("!input.example_data"),
        
        fileInput("upload_expr", 
                  label = h5("Upload gene expression (.csv)",
                             tags$style(type = "text/css", "#q2_data {vertical-align: top;}"),
                             bsButton("q2_data", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                  ),
        bsPopover(id = "q2_data", title = "Upload data",
                  content = "comma-separated-values with samples in columns and gene in rows.",
                  placement = "right", 
                  trigger = "click", 
                  options = list(container = "body")
                  ),
        
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
        ),
        
      )
    ),
    column(8,
      DT::dataTableOutput("expr")  
    )  
  ),
  
### ANALYSIS ###

  hr(),
  fluidRow(
    h3("Select analysis and specific parameters"),
    br(),

#Dorothea ------------------
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
#Progeny ----------------------------
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
                    content = " Target(s) of perturbation. If a list of targets is provided, a comma-separated-values with HGNC symbols or uniprot ids is required.",
                    placement = "right", 
                    trigger = "click", 
                    options = list(container = "body")
          ),
          conditionalPanel(
            condition =  ("input.inputs_targets == 'up'"),
            fileInput("upload_targets", 
                      label = NULL)
            )  
        ),
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
                         content = "Generate a signed and directed network using Omnipath. If a network is upload, a tab-separated file with three headers (Source, Interaction, Target) is required. The network is based on genes, when it contains HGNC symbols, or proteins, when it contains uniprot ids.",
                         placement = "right", 
                         trigger = "click", 
                         options = list(container = "body")
               ),
               conditionalPanel(
                 condition = ("input.omnipath == 'up'"),
                 fileInput("upload_network", label = NULL)),
               radioButtons("net_type", 
                            label = NULL, 
                            choices = c("Gene" = "gene", "Protein" = "protein"), 
                            selected = "gene",
                            inline = TRUE),
               checkboxInput("net_complex", 
                            label = "Add complexes",
                            value = TRUE)
        ),
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
                         content = "Calculate DoRothEA normalised enrichment scores based on the above selected parameters. Alternatively, a file with tab-separeted activites is requiered.",
                         placement = "right", 
                         trigger = "click", 
                         options = list(container = "body")
               ),
               conditionalPanel(
                 condition = ("input.dorothea == 'up'"),
                 fileInput("upload_tfs", label = "Upload TF's activities")),
        ),
        column(2, align="center",
               radioButtons("progeny", 
                            label = h5("Measurments", 
                                       tags$style(type = "text/css", "#q2c_proge {vertical-align: top;}"),
                                       bsButton("q2c_proge", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                            choices = c("PROGENy" = "prog", "Upload" = "up"), 
                            selected = "prog",
                            inline = TRUE),
               bsPopover(id = "q2c_proge", 
                         title = "Measurments",
                         content = "Calculate PROGENy scores based on the above selected parameters. Alternatively, a file with tab-separeted scores (ranged between -1 and 1) per gene is requiered.",
                         placement = "right", 
                         trigger = "click", 
                         options = list(container = "body")
               ),
               conditionalPanel(
                 condition = ("input.progeny == 'up'"),
                 fileInput("upload_progeny", label = "Upload measurments"))
        ),
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
                         content = "Select solver to run the optimization. When cbc/cplex is selected, a path to the execubable file is requiered.",
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
               actionButton("run_carnival", "Run CARNIVAL")
                )
      
      )
           
    )
  )
)
