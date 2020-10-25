
library(shiny)
library(shinyjs)
library(shinythemes)
library(dygraphs)
library(htmlwidgets)

ui <- fluidPage(
  theme = shinytheme('yeti'),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  verbatimTextOutput('info'),
  inputPanel(
    align = "center",
    dateInput('startDate', 'Current Date'),
    dateInput('endDate', 'Retirement Date', value = "2049-12-31"),
    numericInput('initialInvestments', 'Initial Investments', 0, min = 0),
    numericInput('investments', 'Monthly Contributions', 0, min = 0),
    sliderInput('growth', 'Investment Annual Growth',
                min = 0, max = 10, value = 7, step = 0.1, post = '%'),
    actionButton('simulateClick', 'Simulate Contributions', style = "vertical-align: 'middle'")
  ),
  verbatimTextOutput('investmentsAtRetirement'),
  dygraphOutput('investmentGrowth'),
  br(),
  inputPanel(
    align = "center",
    sliderInput('retirementLength', 'Length of retirement',
                min = 0, max = 60, value = 30, step = 1, post = 'years'),
    numericInput('expenses', 'Monthly expenses post-retirement', 4000, min = 0),
    actionButton('simulateRetirementClick', 'Simulate Retirement')
  ),
  verbatimTextOutput('epitaph'),
  dygraphOutput('retirement')
)
