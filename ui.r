library(shiny)

# ui.R

library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File with symbols',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      dateRangeInput('dateRange',
                     label = 'Date Range for Simulation',
                     start = Sys.Date() - 3200, end = Sys.Date()
      ),
      actionButton("goButton", "Go!"),
      p("Click the button to start the simulation.")
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", tableOutput("summary")), 
                  tabPanel("contents", tableOutput("table"))
      )
    )
  )
))