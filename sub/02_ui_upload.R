tabPanel(
  title = "Upload Data",
  # select organism
  selectInput("select_organism", label="Select Organism",
              choices = c("Homo sapiens",
                          "Mus musculus"),
              selected = "Homo sapiens"),
  # Upload gene expression matrix
  fileInput("upload_expr", label="Upload gene expression"),
  #fileInput("upload_pprot", label="Upload phospho-protein expression"),
  switchInput(inputId = "take_example_data", label = "Take example data",
              onLabel = "Yes", offLabel = "No", value=TRUE),
  p("Example dataset taken ",
    a("Blackham et al, J Virol., 2010", 
      href = "https://www.ncbi.nlm.nih.gov/pubmed/20200238"),
    a("(GSE20948)",
      href = "https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi")),
  DT::dataTableOutput("expr"),
  #DT::dataTableOutput("ppomics"),
  bookmarkButton(id = "upload_bookmark"),
  hr()
)