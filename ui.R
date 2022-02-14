library(shiny)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "ProteinSecStruct Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Data", tabName = "data", icon = icon("table")),
    menuItem("Visualisations",tabName = "plots",icon = icon("bar-chart-o")),
    menuItem("Predicted Values", tabName = "pred", icon = icon("table")),
    menuItem("Accuracy", tabName = "accur", icon = icon("table"))
  )),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data",
              h3("Data in pdb_final.csv"),
              tableOutput("contents")
      ),
      # Second tab content
      tabItem(tabName = "plots",
              h3("Visualisation of Attributes"),
              h5("Please allow a few mins for the graphs to load."),
              box(plotOutput("plot1")),
              box(plotOutput("plot2")),
              box(plotOutput("plot4")),
              box(plotOutput("plot3")),
              box(plotOutput("plot5")),
              box(plotOutput("plot6")),
              box(plotOutput("plot7")),
              box(plotOutput("plot8")),
              box(plotOutput("plot9")),
              box(plotOutput("plot10")),
              box(plotOutput("plot11"))
      ),
      # Third tab content
      tabItem(tabName = "pred",
              h3("Predicted values from all the models"),
              tableOutput("predTab")
      ),
      # Fourth tab content
      tabItem(tabName = "accur",
              h3("Accuracy of the models"),
              tableOutput("AccurTab")
      )
    )
  )
)