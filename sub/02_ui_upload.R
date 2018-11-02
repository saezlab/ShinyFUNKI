tabPanel(
  title = "Upload Data",
  # select organism
  selectInput("select_organism", label="Select Organism",
              choices = c("Homo sapiens",
                          "Mus musculus"),
              selected = "Mus musculus"),
  # Upload gene expression matrix
  fileInput("upload_expr", label="Upload gene expression"),
  fileInput("upload_pprot", label="Upload phospho-protein expression"),
  switchInput(inputId = "take_example_data", label = "Take example data",
              onLabel = "Yes", offLabel = "No", value=TRUE),
  DT::dataTableOutput("expr"),
  DT::dataTableOutput("ppomics"),
  hr()
)