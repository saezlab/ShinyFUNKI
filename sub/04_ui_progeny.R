tabPanel(
  title="PROGENy",
  actionButton("run_progeny", label="Run PROGENy"),
  uiOutput("progeny_select_contrast"),
  downloadButton("download_progeny_scores", "Download Pathway-activities"),
  plotOutput("progeny_lollipop"),
  uiOutput("select_pathway"),
  plotOutput("pathway_scatter"),
  numericInput("progeny_selected_top_n_labels",
               label = "Show top 'n' labels", value = 10,
               min = 1, max=100, step=1),
  DT::dataTableOutput("progeny_result"),
  hr()
)