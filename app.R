
library(shiny)
library(lubridate)
library(ggplot2)
library(reshape2)
library(dplyr)

ui <- fluidPage(
  inputPanel( 
    dateInput('startDate', 'Current Date'),
    dateInput('endDate', 'Retirement Date', value = "2049-12-31"),
    numericInput('investments', 'Monthly Investments', 0, min = 0),
    numericInput('savings', 'Monthly Savings', 0, min = 0)
  ),
  h4(verbatimTextOutput('investmentsAtRetirement')),
  h4(verbatimTextOutput('savingsAtRetirement')),
  plotOutput('cumulativeGrowth')
)

server <- function(input, output) {
  n = 80
  growth = 1.07
  simulatedInvestments = reactive({simulateInvestments(input$investments)}) %>% debounce(1500)
  
  monthsToRetire = reactive({
    seq(from = input$startDate, to = input$endDate, by = 'month')
  })
  
  output$savingsAtRetirement = function() {
    paste("savings at retirement",
          (length(monthsToRetire()) - 1) * input$savings
    )
  }
  
  output$investmentsAtRetirement = function() {
    i = simulatedInvestments() %>%
      group_by(variable) %>%
      summarize(max = max(value))
    paste("investments at retirement",
          "\nmin:", min(i$max) %>% round(),
          "\nmax:", max(i$max) %>% round(),
          "\nmedian:", median(i$max) %>% round())
  }
  
  output$cumulativeGrowth = renderPlot({
    simulatedInvestments() %>%
      group_by(variable) %>%
      ggplot(aes(x=time, y=value, color=variable)) +
      geom_line()
  })
  
  nextMonth = function(investmentsByMonth) {
    investmentsByMonth * growth ^ (1/12) * rnorm(1, 1, 0.03)
  }
  
  simulateInvestments = function(investments) {
    investmentsSumTable = list()
    
    for (x in 1:n) {
      investmentsByMonth = c()
      investmentsSum = c()
      for (month in 1:length(monthsToRetire())) {
        investmentsByMonth = c(investmentsByMonth, investments)
        investmentsByMonth = nextMonth(investmentsByMonth)
        investmentsSum = c(investmentsSum, sum(investmentsByMonth))
      }
      investmentsSumTable[[x]] = investmentsSum
    }
    
    z = data.frame(investmentsSumTable)
    colnames(z) = seq(1, n)
    z = cbind(z, time=1:length(monthsToRetire()))
    z = melt(z, id.vars='time')
    return(z)
  }
}

shinyApp(ui = ui, server = server)
