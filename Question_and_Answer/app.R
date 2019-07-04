#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(plotly)
library(leaflet.extras)
library(dplyr)
library(shinyBS)
library(data.table)
library(tidyr)
library(DT)

quesAns <- fread('zendesk_challenge.tsv', encoding='UTF-8', quote = "")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Test Your Knowledge",
                  titleWidth = 400),
  
  
#  dropdownMenu(type = "notifications",
#               notificationItem(
#                 text = "Version Number 01",
#                 icon = icon("laptop-code")
#               )
#  )
#  ),
               
   # Sidebar content 
  dashboardSidebar(
    sidebarMenu(
      
      # Question and Answer tab for main panel
      menuItem("Question and Answer", tabName = "QA", icon = icon("graduation-cap")),
      
      # Data table tab to view data
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      
      # ReadMe tab to view background information
      menuItem("ReadMe", tabName = "readme", icon = icon("book-reader")),
      
      # Code tab to view code
      menuItem("Code", tabName = "code", icon = icon("code")),
      
      hr(),
      br(),
      br(),
      
      menuItem(bsButton("End Play", label = "End Game", style = "warning", 
                        size = "large", type = "action", icon = icon("ban"))),
      tags$p("Click this button finish playing.")
    
    )
  ),
      
      
      dashboardBody(
        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
        
        # tags$head(tags$style(HTML('
        #                       .skin-blue .main-header .logo {
        #                       background-color: #3c8dbc;
        #                       }
        #                       .skin-blue .main-header .logo:hover {
        #                       background-color: #3c8dbc;
        #                       }
        #                       ')
        #                      )
        # ),
        
        tabItems(
          # Question tab content
          tabItem(tabName = "QA",
                  fluidRow(
                    
                    # Box 1. Question
                    box(title = "Question", status = "primary", height = 200, width = 12,
                        solidHeader = TRUE,
                        actionButton("action", label = "Pick Random Question"),
                        verbatimTextOutput("value")),
                    
#                    hr(),
#                    fluidRow(column(2, verbatimTextOutput("value"))),
                    
                    
                    # Box 2. Answers
                    box(title = "Possible Answers", status = "primary", 
                        height = 680, width = 12, solidHeader = TRUE,
                        column(12, tableOutput('table')
                        ))
                    
                  )
          ),
          
          # Data Table tab content
          tabItem(tabName = "table",
                  fluidPage(title = "Data Table",
                            div(dataTableOutput("dataTable"), 
                                style = "font-size: 100%; width: 100%"))
          ),
          
          # ReadMe tab content
          tabItem(tabName = "readme",
                  fluidRow(includeMarkdown("readMe.Rmd")
                  )
          ),
          
          # Code tab content
          tabItem(tabName = "code",
                  fluidRow(includeMarkdown("code.Rmd")
                  )
          )
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderTable(filter(quesAns, DocumentTitle == "Apollo Creed"))

    # You can access the value of the widget with input$action, e.g.
#    output$value <- renderPrint({ input$action })
  observeEvent(input$action, {
    newtab <- switch(input$tabs, 
                     "CustomerRetention" = "Marketing",
                     "Marketing" = "CustomerRetention")
    
    sample_n(quesAns, 1)
  })
    
    
    
   output$dataTable <- DT::renderDataTable(quesAns)
}

# Run the application 
shinyApp(ui = ui, server = server)

