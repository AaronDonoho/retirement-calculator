
library(glue)
library(reshape2)
library(dplyr)

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

FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"

nextMonth = function(investments, growthPercent) {
  if (investments <= 1) {
    return(0)
  }
  growthRate = growthPercent / 100
  investments * min(1.35, max(.75, rnorm(1, (1 + growthRate), growthRate * 3))) ^ (1/12) 
}

runSimulation <- function(initialFunds, contribution, monthCount, growth) {
  investmentsSum = c(initialFunds)
  for (month in 1:monthCount) {
    investmentsSum[length(investmentsSum)] = max(0, investmentsSum[length(investmentsSum)] + contribution)
    nextMonth = nextMonth(investmentsSum[length(investmentsSum)], growth)
    investmentsSum = c(investmentsSum, nextMonth)
  }
  return(investmentsSum)
}

toTimeSeriesTable <- function(investmentsSumTable, simulationCount, monthCount) {
  data.frame(investmentsSumTable) %>%
    `colnames<-`(seq(1, simulationCount)) %>%
    cbind(time = 0:monthCount) %>%
    reshape2::melt(id.vars='time') %>%
    return()
}

simulateEachFund = function(initialFunds, contribution, monthCount, growth) {
  initialFunds %>%
    lapply(runSimulation, contribution, monthCount, growth) %>%
    toTimeSeriesTable(length(initialFunds), monthCount) %>%
    return()
}
