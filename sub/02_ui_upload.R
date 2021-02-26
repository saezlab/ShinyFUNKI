tabPanel(
  title = "Data and Parameters",
  fluidRow(
    h3("Upload Data"),
    column(4,
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
      
      #show the upload stage if the example is selected
      conditionalPanel(
        condition = ("!input.example_data"),
        
        # Upload data
        fileInput("upload_expr", 
                  label = "Upload gene expression",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # select type of data gene expression matrix or contrast
        radioButtons("data_type", "Type of data",
                     choices = c("Gene expression matrix" = "gex",
                                 Contrast = "contrast"),
                     selected = "gex"),
        
        # select organism
        selectInput("select_organism", label="Select Organism",
                    choices = c("Homo sapiens",
                                "Mus musculus"),
                    selected = "Homo sapiens")
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
    
    column(1, align="center", 
      img(src="logo_dorothea.png", align = "right", height=75, width=75)
    ),
    column(11,
      fluidRow(
        column(3, 
          awesomeCheckboxGroup(inputId = "selected_conf_level",
                                  label = "Select Confidence Level",
                                  choices = c("A", "B", "C", "D", "E"),
                                  selected = c("A","B", "C"),
                                  inline = TRUE)
        ),
        column(3,
               sliderInput(inputId = "minsize", label = "Regulon's minimal size", min = 1, max = 300, value = 5)
        ),
        column(3,
               selectInput(inputId = "method", label = "Method for computing signatures",
                           choices = c("scale" = "scale",
                                       "rank" = "rank", 
                                       "mad" = "mad",
                                       "ttest" = "ttest", 
                                       "none" = "none"),
                           selected = "none")
        ),
        column(2,
               actionButton("an_dorothea", "Run DoRothEA") )
      )
    )
  ),
  hr(),
  fluidRow(
    column(1, align="center", 
      img(src="logo_progeny.png", align = "right", height=75, width=120)
    ),
    column(11,
      fluidRow(
        column(4,
               sliderInput(inputId = "perm", label = "Number of permutations", min = 1, max = 300, value = 100)
        ),
        column(4,
               sliderInput(inputId = "top", label = "Top genes for model matrix", min = 1, max = 300, value = 100)
        ),
        column(2,
               actionButton("an_progeny", "Run PROGENy")
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(1, align="center", 
      img(src="logo_CARNIVAL.png", align = "right", height=75, width=75)
    ),
    column(11,
      fluidRow(
        column(3,
          selectInput(inputId = "inputs_targets", label = "Targets",
                      choices = c("All posible from network" = "all_inputs",
                                  "Let CARNIVAL choose them" = "inverse", 
                                  "My own list of targets" = "up"),
                      selected = "inverse"),
          conditionalPanel(
            condition =  ("input.inputs_targets == 'up'"),
            fileInput("upload_network", label = NULL))  
        ),
        column(2,
               radioButtons("omnipath", label = "Network", 
                            choices = c("Omnipath" = "omni", "Upload" = "up"), 
                            selected = "omni",
                            inline = TRUE),
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
        column(2,
               radioButtons("dorothea", label = "TF's activities", 
                            choices = c("DoRothEA" = "doro", "Upload" = "up"), 
                            selected = "doro",
                            inline = TRUE),
               conditionalPanel(
                 condition = ("input.dorothea == 'up'"),
                 fileInput("upload_tfs", label = "Upload TF's activities")),
        ),
        column(2,
               radioButtons("progeny", label = "Measurments", 
                            choices = c("PROGENy" = "prog", "Upload" = "up"), 
                            selected = "prog",
                            inline = TRUE),
               conditionalPanel(
                 condition = ("input.progeny == 'up'"),
                 fileInput("upload_progeny", label = "Upload measurments"))
        ),
        column(3,
               radioButtons("solver", label = "Solver", 
                            choices = c("lpSolve" = "lpSolve", "cplex" = "cplex", "cbc" = "cbc"), 
                            selected = "cplex",
                            inline = TRUE),
               conditionalPanel(
                 condition = ("input.solver != 'lpSolve'"),
                 fileInput("solverPath", label = "Select path to execute cbc/cplex file")),
               actionButton("run_carnival", "Run CARNIVAL")
                )
      
      )
           
    )
  )
)
