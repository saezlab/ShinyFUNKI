tabPanel(
  title = "Integration",
  actionButton("run_cor_pw_tf", label="Cor PW-TF"),
  plotOutput("network_pw_tf"),
  DT::dataTableOutput("cor_result")
)