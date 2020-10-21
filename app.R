
library(shiny)
library(shinythemes)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(glue)

n = 150

info = 
glue::glue(
"Assumptions:
  Growth has some random variance within a normal distribution
  Higher growth means higher risk; this results in a wider spread in simulations
  Value is always expressed in today's terms
  {n} simulations are run
")

nextMonth = function(investmentsByMonth, growthPercent) {
  growthRate = growthPercent / 100
  investmentsByMonth * rnorm(1, (1 + growthRate), growthRate * 2) ^ (1/12) 
}

simulateInvestments = function(initial, investments, monthsToRetire, growth) {
  investmentsSumTable = list()
  
  for (x in 1:n) {
    investmentsByMonth = c(initial)
    investmentsSum = c(initial)
    for (month in 1:length(monthsToRetire)) {
      investmentsByMonth = c(investmentsByMonth, investments)
      investmentsByMonth = nextMonth(investmentsByMonth, growth)
      investmentsSum = c(investmentsSum, sum(investmentsByMonth))
    }
    investmentsSumTable[[x]] = investmentsSum
  }
  
  z = data.frame(investmentsSumTable)
  colnames(z) = seq(1, n)
  z = cbind(z, time=0:length(monthsToRetire))
  z = melt(z, id.vars='time')
  return(z)
}

ui <- fluidPage(
  theme = shinytheme('yeti'),
  inputPanel( 
    dateInput('startDate', 'Current Date'),
    dateInput('endDate', 'Retirement Date', value = "2049-12-31"),
    numericInput('initialSavings', 'Initial Savings (-Debt)', 0),
    numericInput('savings', 'Monthly Savings', 0, min = 0),
    numericInput('initialInvestments', 'Initial Investments', 0, min = 0),
    numericInput('investments', 'Monthly Investments', 0, min = 0),
    sliderInput('growth', 'Investment Annual Growth',
                min = 0, max = 10, value = 7, step = 0.1, post = '%')
  ),
  verbatimTextOutput('savingsAtRetirement'),
  verbatimTextOutput('investmentsAtRetirement'),
  plotOutput('investmentGrowth'),
  verbatimTextOutput('info')
)

server <- function(input, output) {
  
  monthsToRetire = reactive({
    req(input$endDate > input$startDate)
    seq(from = input$startDate, to = input$endDate, by = 'month')
  })
  
  simulatedInvestments = reactive({
    req(input$initialInvestments, input$investments, monthsToRetire(), input$growth)
    simulateInvestments(input$initialInvestments, input$investments, monthsToRetire(), input$growth)
  }) %>% debounce(1500)
  
  simulatedSavings = reactive({
    req(input$initialSavings, input$savings, monthsToRetire())
    (input$initialSavings + (length(monthsToRetire()) - 1) * input$savings)
  }) %>% debounce(1500)
  
  output$savingsAtRetirement = function() {
    paste0("savings at retirement: $", simulatedSavings() %>% format(big.mark=",")
    )
  }
  
  output$investmentsAtRetirement = function() {
    i = simulatedInvestments() %>%
      group_by(variable) %>%
      filter(time == max(time)) %>%
      select(value)
    q = quantile(i$value, c(0.1, 0.5, 0.9))
    paste0("simulated investments at retirement",
          "\n10th percentile: $", q[1] %>% round() %>% format(big.mark=","),
          "\n50th percentile: $", q[2] %>% round() %>% format(big.mark=","),
          "\n90th percentile: $", q[3] %>% round() %>% format(big.mark=","))
  }
  
  output$investmentGrowth = renderPlot({
    simulatedInvestments() %>%
      group_by(variable) %>%
      ggplot(aes(x=time, y=value, color=variable)) +
      geom_line(alpha=0.4) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(legend.position="none")
  })
  
  output$info = function() {
    info
  }
}

shinyApp(ui = ui, server = server)
