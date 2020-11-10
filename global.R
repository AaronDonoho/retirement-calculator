
library(shiny)
library(shinyjs)
library(shinyFeedback)
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
    - Growth has random variance within a Laplace distribution
    - Higher growth results in higher variance
    - A steady inflation of 2.5% is used to devalue investments
    - During retirement, excesss supplemental income is invested
    - Currency is always expressed in terms of current value
    - {n} simulations are run

    Instructions for Pre-Retirement Simulator:
    1. Enter age and expected retirement age
    2. Enter current investments and monthly contributions
    3. Select a rate of growth matching your investment portfolio
    4. Click the 'Simulate Contributions' button
       A graph will display. Each line is a different simulation.

    Instructions for Post-Retirement Simulator:
    1. Enter life expectancy
    2. Enter expected monthly expenses in retirement
    3. Enter age for supplementary income (e.g. Social Security),
       and expected income amount (make sure it's inflation-adjusted)
    3. Select a rate of growth matching your investment portfolio
    4. Click the 'Simulate Retirement' button
       A graph will display. Each line is a different simulation.
    ")

FUNC_JSFormatNumber <- "function(x) {return x.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1,')}"

validNumber = function(input) {
  return(!is.na(input) && input >= 0)
}

validAge = function(input) {
  return(!is.na(input) && input >= 0 && input <= 120)
}

validateAtLeast = function(input, inputName, value) {
  hideFeedback(inputName)
  if (!validNumber(input) || input < value) {
    showFeedbackDanger(inputName, text = paste("must be at least", value))
    return(F)
  }
  return(T)
}

validateAge = function(input, inputName) {
  hideFeedback(inputName)
  if (!validAge(input)) {
    showFeedbackDanger(inputName, text = "must be between 0 and 120")
    return(F)
  }
  return(T)
}

validateAgeAtLeast = function(input, inputName, compareInput, value) {
  hideFeedback(inputName)
  if (!validAge(input)) {
    showFeedbackDanger(inputName, text = "must be between 0 and 120")
    return(F)
  } else if (validAge(compareInput) && input < compareInput) {
    showFeedbackDanger(inputName, text = paste("must be at least", value))
    return(F)  
  }
  return(T)
}

validateAgeGreaterThan = function(input, inputName, compareInput, value) {
  hideFeedback(inputName)
  if (!validAge(input)) {
    showFeedbackDanger(inputName, text = "must be between 0 and 120")
    return(F)
  } else if (validAge(compareInput) && input <= compareInput) {
    showFeedbackDanger(inputName, text = paste("must be greater than", value))
    return(F)  
  }
  return(T)
}

assetClassForGrowth = function(growth) {
  text = "stock index funds"
  if (growth == 0) {
    text = "cash"
  } else if (growth < 1) {
    text = "savings"
  } else if (growth < 4) {
    text = "savings/bonds mix"
  } else if (growth < 6) {
    text = "bond index funds"
  } else if (growth < 9) {
    text = "bond/stock mix"
  }
  return(text)
}

monthsBetweenYears = function(start, end) {
  seq(from = 12 * start, to = 12 * end - 1)
}

rlaplace = function(x, location, scale) {
  sign = -1
  if (rbinom(x, 1, .5) > .5) {
    sign = 1
  }
  location + sign * scale / sqrt(2) * log(1 - runif(x, 0, 1))
}

nextMonth = function(investments, growthPercent) {
  if (investments <= 1) {
    return(0)
  }
  growthRate = growthPercent / 100
  # change = rnorm(1, (1 + growthRate), 2 * growthRate ^ 1.2)
  change = rlaplace(1, (1 + growthRate), 2 * growthRate ^ 1.2)
  
  investments * devaluation * min(10, max(.1, change)) ^ (1 / 12)
}

simulateFund <- function(initialFunds, contribution, monthCount, growth) {
  investmentsSum = c(initialFunds)
  for (month in 2:(monthCount + 1)) {
    investmentsSum = c(
      investmentsSum,
      max(0, nextMonth(investmentsSum[month - 1], growth) + contribution)
    )
  }
  return(investmentsSum)
}

toTimeSeriesTable <- function(investmentsSumTable, simulationCount, monthCount) {
  data.frame(investmentsSumTable) %>%
    `colnames<-`(seq(1, simulationCount)) %>%
    cbind(time = 0:(monthCount)) %>%
    reshape2::melt(id.vars='time')
}

simulateEachFund = function(initialFunds, contribution, monthCount, growth) {
  initialFunds %>%
    lapply(simulateFund, contribution, monthCount, growth) %>%
    toTimeSeriesTable(length(initialFunds), monthCount)
}

investmentGraph <- function(data, yRange, title) {
  dygraph(data, main = title) %>%
    dyAxis("y", label = "Value of Investments", valueRange = yRange, axisLabelWidth=80,
           axisLabelFormatter=htmlwidgets::JS(FUNC_JSFormatNumber),
           valueFormatter=htmlwidgets::JS(FUNC_JSFormatNumber)) %>%
    dyAxis("x", label = "Age", drawGrid = F) %>%
    dyLegend(show = "never") %>%
    dyOptions(colors = c("#0077b6","#593f62","#fe5f55", "#18f2b2", "#002A22",
                         "#fcbf49", "#F6F740", "#04724D", "#A67DB8", "#A40E4C")) %>%
    dyHighlight(highlightCircleSize = 1,
                highlightSeriesBackgroundAlpha = 0.3)
}
