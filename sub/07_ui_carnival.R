tabPanel(
  title="CARNIVAL",
  fluidRow(
    column(
      12, align="center", 
      sidebarLayout(
        sidebarPanel(
          width = 12,
          fluidRow(
            # column(6, align = "center",
            #        uiOutput("select_node")
            #        ),
            # column(6, align = "center",
            #        uiOutput("select_node")
            # ),
            
            checkboxInput("hierarchical", label = "Hierarchical layout", value = FALSE)
            
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
 
  hr()
)