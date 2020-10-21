
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
    "
This retirement calculator may help determine how much
to save and spend to meet your retirement goals.

Assumptions:
  Growth has some random variance within a normal distribution
  Higher growth means higher risk; this results in a wider spread in simulations
  Value is always expressed in today's terms
  {n} simulations are run
")

nextMonth = function(investments, growthPercent) {
  if (investments <= 1) {
    return(0)
  }
  growthRate = growthPercent / 100
  investments * rnorm(1, (1 + growthRate), growthRate * 2) ^ (1/12) 
}

simulateInvestments = function(initial, investments, monthCount, growth) {
  investmentsSumTable = list()
  
  for (x in 1:n) {
    investmentsByMonth = c(initial)
    investmentsSum = c(initial)
    for (month in 1:monthCount) {
      investmentsSum[length(investmentsSum)] = last(investmentsSum) + investments
      nextMonth = nextMonth(last(investmentsSum), growth)
      investmentsSum = c(investmentsSum, nextMonth)
    }
    investmentsSumTable[[x]] = investmentsSum
  }
  
  z = data.frame(investmentsSumTable)
  colnames(z) = seq(1, n)
  z = cbind(z, time=0:monthCount)
  z = melt(z, id.vars='time')
  return(z)
}

simulateExpenses = function(initial, expenses, monthCount, growth) {
  investmentsSumTable = list()
  
  for (i in 1:length(initial)) {
    investmentsSum = c(initial[i])
    for (month in 1:monthCount) {
      investmentsSum[length(investmentsSum)] = max(0, last(investmentsSum) - expenses)
      nextMonth = nextMonth(last(investmentsSum), growth)
      investmentsSum = c(investmentsSum, nextMonth)
    }
    investmentsSumTable[[i]] = investmentsSum
  }
  
  z = data.frame(investmentsSumTable)
  colnames(z) = seq(1, length(initial))
  z = cbind(z, time=0:monthCount)
  z = melt(z, id.vars='time')
  return(z)
}

ui <- fluidPage(
  theme = shinytheme('yeti'),
  verbatimTextOutput('info'),
  inputPanel( 
    dateInput('startDate', 'Current Date'),
    dateInput('endDate', 'Retirement Date', value = "2049-12-31"),
    numericInput('initialInvestments', 'Initial Investments', 0, min = 0),
    numericInput('investments', 'Monthly Investments', 0, min = 0),
    sliderInput('growth', 'Investment Annual Growth',
                min = 0, max = 10, value = 7, step = 0.1, post = '%')
  ),
  verbatimTextOutput('investmentsAtRetirement'),
  h3('Investment growth prior to retirement'),
  plotOutput('investmentGrowth'),
  inputPanel(
    numericInput('expenses', 'Monthly expenses post-retirement', 4000, min = 0),
    sliderInput('retirementLength', 'Length of retirement',
                min = 0, max = 100, value = 30, step = 1, post = 'years')
  ),
  h3('Investment draw down after retirement'),
  plotOutput('retirement'),
  verbatimTextOutput('epitaph')
)

server <- function(input, output) {
  
  monthsToRetire = reactive({
    req(input$endDate > input$startDate)
    seq(from = input$startDate, to = input$endDate, by = 'month')
  })
  
  simulatedRetirement = reactive({
    req(input$expenses, input$retirementLength)
    initial = simulatedInvestments() %>%
      filter(time == max(time)) %>%
      select(value) %>%
      t() %>%
      as.vector()
    
    simulateExpenses(initial, input$expenses, input$retirementLength * 12, input$growth)
  }) %>% debounce(1500)
  
  simulatedInvestments = reactive({
    req(input$initialInvestments, input$investments, monthsToRetire(), input$growth)
    simulateInvestments(input$initialInvestments, input$investments, length(monthsToRetire()), input$growth)
  }) %>% debounce(1500)
  
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
  
  output$retirement = renderPlot({
    simulatedRetirement() %>%
      group_by(variable) %>%
      ggplot(aes(x=time, y=value, color=variable)) +
      geom_line(alpha=0.4) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_bw() +
      theme(legend.position="none")
  })
  
  output$epitaph = function() {
    s = simulatedRetirement() %>%
      group_by(variable) %>%
      filter(time == max(time)) %>%
      dplyr::ungroup() %>%
      select(value)
    q = quantile(s$value, c(0.1, 0.5, 0.9))
    s %<>% t() %>% as.vector()
    pass_percent = (sum(s > 0) / length(s)) %>% round(2)
      
    paste0("simulated investments at end of retirement",
           "\n10th percentile: $", q[1] %>% round() %>% format(big.mark=","),
           "\n50th percentile: $", q[2] %>% round() %>% format(big.mark=","),
           "\n90th percentile: $", q[3] %>% round() %>% format(big.mark=","),
           "\n", pass_percent * 100, "% of simulations had money remaining at the end")
  }
  
  output$info = function() {
    info
  }
}

shinyApp(ui = ui, server = server)
