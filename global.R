
library(shiny)
library(shinyjs)
library(glue)
library(reshape2)
library(dplyr)
library(htmlwidgets)
library(dygraphs)

n = 150
devaluation = (1 / 1.025) ^ (1/12)

info = 
  glue::glue(
    "
    This retirement calculator may help determine how much
    to save and spend to meet your retirement goals.
    
    Assumptions:
    Growth has random variance within a normal distribution
    Higher growth results in higher variance
    A steady inflation of 2.5% is used to devalue investments
    Currency is always expressed in terms of current value
    {n} simulations are run
    ")

FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"

nextMonth = function(investments, growthPercent) {
  if (investments <= 1) {
    return(0)
  }
  growthRate = growthPercent / 100
  investments * devaluation * min(10, max(.1, rnorm(1, (1 + growthRate), 2.5*growthRate^1.2))) ^ (1/12)
}

simulateFund <- function(initialFunds, contribution, monthCount, growth) {
  investmentsSum = c(initialFunds)
  for (month in 1:monthCount) {
    investmentsSum[month] = 
      max(0, investmentsSum[month] + contribution)
    nextMonth = nextMonth(investmentsSum[month], growth)
    investmentsSum = c(investmentsSum, nextMonth)
  }
  return(investmentsSum)
}

toTimeSeriesTable <- function(investmentsSumTable, simulationCount, monthCount) {
  data.frame(investmentsSumTable) %>%
    `colnames<-`(seq(1, simulationCount)) %>%
    cbind(time = 0:monthCount) %>%
    reshape2::melt(id.vars='time')
}

simulateEachFund = function(initialFunds, contribution, monthCount, growth) {
  initialFunds %>%
    lapply(simulateFund, contribution, monthCount, growth) %>%
    toTimeSeriesTable(length(initialFunds), monthCount)
}

investmentGraph <- function(data, yRange, title) {
  dygraph(data, main = title) %>%
    dyAxis("y", label = "Value", valueRange = yRange, axisLabelWidth=80,
           axisLabelFormatter=htmlwidgets::JS(FUNC_JSFormatNumber),
           valueFormatter=htmlwidgets::JS(FUNC_JSFormatNumber)) %>%
    dyAxis("x", label = "Time", drawGrid = F) %>%
    dyLegend(show = "never") %>%
    dyOptions(colors = c("#0077b6","#593f62","#fe5f55", "#18f2b2", "#002A22",
                         "#fcbf49", "#F6F740", "#04724D", "#A67DB8", "#A40E4C")) %>%
    dyHighlight(highlightCircleSize = 1,
                highlightSeriesBackgroundAlpha = 0.3)
}
