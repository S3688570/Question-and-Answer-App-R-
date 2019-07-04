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
library(rhandsontable)

#num <-  c(0,1)
quesAns <- fread('zendesk_challenge.tsv', encoding='UTF-8', quote = "")

#quesAns_rand <- rhandsontable(quesAns) %>% hot_col(col = "Label", 
#                  type = "dropdown", source = num, allowInvalid = TRUE)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Test Your Knowledge",
                  titleWidth = 400),
  
  
#  dropdownMenu(type = "notifications",
#               notificationItem(
#                 text = "Version Number 01",
#                 icon("laptop-code")
#               )
#  ),
               
   # Sidebar content 
  dashboardSidebar(
    sidebarMenu(
      
      # Question and Answer tab for main panel
      menuItem("Question and Answer", tabName = "QA", icon = icon("graduation-cap")),
      
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
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

        
        tabItems(
          # Question tab content
          tabItem(tabName = "QA",
                  fluidRow(
                    
                    # Box 1. Question
                    box(title = "Question", status = "primary", height = 200, width = 12,
                        solidHeader = TRUE,
                        helpText("1. When you click the button to see",
                                 "a random question."),
                        actionButton("action", label = "Pick Random Question"),
                        
                        verbatimTextOutput("questionSample"),
                        helpText("2. Click on the best answer in the table below.")
                ),
                    
#                    hr(),
#                    fluidRow(column(2, verbatimTextOutput("value"))),
                    
                    
                    # Box 2. Answers
                    box(title = "Possible Answers", status = "primary", 
                        height = 680, width = 12, solidHeader = TRUE,
                        column(12, DT::DTOutput('answerSample')),
                        column(3, verbatimTextOutput('pick'))
                    )
                )
          ),
          
          # Data Table tab content
          tabItem(tabName = "table",
                  fluidPage(
                    titlePanel("Question and Answer Dataset"),
                    fluidRow(
                      #Create a new row for selectInputs
                      column(4,
                             selectInput("DT", "Document Title:", 
                                         c("All", unique(as.character(quesAns$DocumentTitle)))),
                             
                      #Create a new row for the table
                            dataTableOutput("dataTable"), 
                                style = "font-size: 100%; width: 100%")
                  )
                )
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
  
  filtered_data <- eventReactive(input$action, {
    # Filter to find random Question 
    samp <- unique(quesAns$Question)
  samp
  })
  
  output$questionSample <- renderPrint({
    filt_random_sample <- filtered_data()
    filt_random_sample <- sample(filt_random_sample, 1, replace = FALSE)
    filt_random_sample
  })
  
  output$answerSample <- DT::renderDataTable({
    filt_random_sample <- filtered_data()
    filt_random_sample <- sample(filt_random_sample, 1, replace = TRUE)
    filt_random_sample <- filter(quesAns, quesAns$Question == filt_random_sample)
    filt_random_sample <- filt_random_sample[,5:6]
    filt_random_sample$rownames <- seq(from = 1, to = length(filt_random_sample[,1]), by = 1)
    filt_random_sample <- column_to_rownames(filt_random_sample, var = "rownames")
    filt_random_sample}
    , server = TRUE
  )
  
  output$pick = renderPrint({
   s = input$answerSample_row_selected
   if (length(s)) {
     cat('Correct Answer')
     # if (quesAns[s,"Label"] == 1) {
     #   cat('Correct Answer')
     # }
   }
      # else if (quesAns[s,"Label"] == 0){
      #   cat('Sorry that pick is not correct. \n\n')
      #   cat('Pick another question.')
      # } else {
      #   cat('Pick another question')
      # }
  })
  
  
  
  output$table <- renderTable(filter(quesAns, DocumentTitle == "Apollo Creed"))
    
    
  #Filter table based on selections
   output$dataTable <- DT::renderDataTable(DT::datatable({
     data <- quesAns
     if (input$DT != "All") {
       data <- data[data$DocumentTitle == input$DT,]
     }
     data
   }))
}

# Run the application 
shinyApp(ui = ui, server = server)

