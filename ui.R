
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme('yeti'),
  useShinyFeedback(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  verbatimTextOutput('info'),
  inputPanel(
    align = "center",
    numericInput('currentAge', 'Current Age', 35, min = 0, max = 120),
    numericInput('retirementAge', 'Retirement Age', 60, min = 0, max = 120),
    numericInput('initialInvestments', 'Initial Investments', 0, min = 0),
    numericInput('contributions', 'Monthly Contributions', 0, min = 0),
    sliderInput('investingGrowth', 'Investment Annual Growth',
                min = 0, max = 10, value = 9, step = 0.1, post = '%'),
    actionButton('simulateClick', 'Simulate Contributions', style = "vertical-align: 'middle'")
  ),
  verbatimTextOutput('investmentsAtRetirement'),
  dygraphOutput('investmentGrowth'),
  br(),
  inputPanel(
    align = "center",
    numericInput('lifeExpectancy', 'Life Expectancy', 100, min = 0, max = 120),
    numericInput('additionalIncomeAge', 'Age at Start of Supplement', 62.5, min = 0, max = 120),
    numericInput('expenses', 'Monthly Expenses In Retirement', 0, min = 0),
    numericInput('additionalIncome', 'Supplemental Income', 0, min = 0),
    sliderInput('retirementGrowth', 'Investment Annual Growth',
                min = 0, max = 10, value = 6, step = 0.1, post = '%'),
    actionButton('simulateRetirementClick', 'Simulate Retirement')
  ),
  verbatimTextOutput('epitaph'),
  dygraphOutput('retirement')
)
