library(shiny)

# ui.R
# checking the commit

library(shiny)

shinyUI(fluidPage(
  titlePanel("Uploading Files"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File with symbols',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
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
      tags$hr(),
      textInput("benchmark", label = "Benchmark Symbol Bitte!!", value = "^GSPC"),
      tags$hr(),
      sliderInput("emawin", "Moving Average Window:", 
                  min = 1, max = 50, value = 12, step= 1),
      tags$hr(),
      sliderInput("adj", "Adjustment Coefficient:", 
                  min = 0.1, max = 0.5, value = 0.3, step= 0.1),
      tags$hr(),
      selectizeInput('adjfre', 'Adjustment Frequency(NA)', choices = setNames(c("YR", "QR", "WK"), 
                                                          c("Yearly", "Quaterly", "Weekly"))),
      tags$hr(),
      sliderInput("txncost", "Transaction Cost (Basis Points):", 
                  min = 1, max = 20, value = 2, step= 1),
      tags$hr(),
      dateRangeInput('dateRange',
                     label = 'Date Range for Simulation',
                     start = Sys.Date() - 3200, end = Sys.Date()
      ),
      tags$hr(),
      p("Click the button to start the simulation."),
      actionButton("goButton", "Go!")
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