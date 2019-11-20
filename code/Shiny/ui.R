library(shiny)
library(ggplot2)
library(tm)
library(shinythemes)
library(shinydashboard)
library(dashboardthemes)

shinyUI(dashboardPage(
  dashboardHeader(title = "Improve your seafood businesses!",
                  titleWidth = 300),
  dashboardSidebar(
      width = 300,
      textInput('bus',
                label = 'Business-id',
                value = "nsNONDHbV7Vudqh21uicqw"
      ),
      sidebarMenu(
        (menuItem(" If you have questions, please email to:")),
        (menuItem(" sshen82@wisc.edu")),
        (menuItem(" phe25@wisc.edu")),
        (menuItem(" zgao92@wisc.edu")),
        (menuItem(" awong43@wisc.edu"))
      )
    ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
          .content-wrapper {
            background-color: white !important;
          }
          .main-sidebar {
            background-color: #2B5F75 !important;
            font-size: 15px;
          }
        "))
    ),
    tabsetPanel(
      tabPanel("Review Analysis",
               fluidRow(
                 column(width = 6,
                        h4(helpText("This is the rank of aspects you should enhance.")),
                        tableOutput("ref"),
                        br(),
                        h3(htmlOutput("conclusion")),
                        h3(htmlOutput("plotservice")),
                        h3(htmlOutput("plotfood"))),
                 column(width = 6,
                        h4(helpText("You can compare with other restaurants using this boxplot.")),
                        plotOutput("plot"))
               )
      ),
      tabPanel("Attribute Analysis",
        h3(htmlOutput("att1")),
        h3(htmlOutput("att2")),
        h3(htmlOutput("att3")),
        h3(htmlOutput("att4")),
        h3(htmlOutput("att5")),
        h3(htmlOutput("att6")),
        h3(htmlOutput("att7"))
      )
    )
  )
)
)

