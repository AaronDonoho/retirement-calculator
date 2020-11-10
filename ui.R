
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme('yeti'),
  useShinyFeedback(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  uiOutput('info'),
  h2("Pre-Retirement"),
  inputPanel(
    align = "center",
    numericInput('currentAge', 'Current Age', 35, min = 0, max = 120),
    numericInput('retirementAge', 'Retirement Age', 60, min = 0, max = 120),
    numericInput('initialInvestments', 'Initial Investments', 0, min = 0),
    numericInput('contributions', 'Monthly Contributions', 0, min = 0),
    sliderInput('preRetirementGrowth', 'Investment Annual Growth',
                min = 0, max = 10, value = 8, step = 0.1, post = '%'),
    actionButton('simulatePreClick', 'Simulate Pre-Retirement', style = "vertical-align: 'middle'")
  ),
  uiOutput('investmentsAtRetirement'),
  dygraphOutput('investmentGrowth'),
  br(), br(), br(),
  h2("Post-Retirement"),
  inputPanel(
    align = "center",
    numericInput('lifeExpectancy', 'Life Expectancy', 100, min = 0, max = 120),
    numericInput('expenses', 'Monthly Expenses In Retirement', 0, min = 0),
    numericInput('additionalIncomeAge', 'Age at Start of Supplement', 62, min = 0, max = 120),
    numericInput('additionalIncome', 'Supplemental Income', 0, min = 0),
    sliderInput('postRetirementGrowth', 'Investment Annual Growth',
                min = 0, max = 10, value = 5, step = 0.1, post = '%'),
    actionButton('simulatePostClick', 'Simulate Post-Retirement')
  ),
  uiOutput('epitaph'),
  dygraphOutput('retirement')
)
