
library(dygraphs)
library(lubridate)

server <- function(session, input, output) {
  simulatedInvestments = reactiveVal(NULL)
  simulatedRetirement = reactiveVal(NULL)
  
  disableSimulations = function() {
    shinyjs::disable('simulateClick')
    shinyjs::disable('simulateRetirementClick')
  }
  
  enableSimulations = function() {
    shinyjs::enable('simulateClick')
    shinyjs::enable('simulateRetirementClick')
  }
  
  simulateClick = reactive({
    input$simulateClick
  })
  
  simulateRetirementClick = reactive({
    input$simulateRetirementClick
  })
  
  repairInputsForContributions = function() {
    allValid = all(
      validateAtLeast(input$initialInvestments, "initialInvestments", 0),
      validateAtLeast(input$contributions, "contributions", 0),
      validateAge(input$currentAge, "currentAge"),
      validateAgeGreaterThan(input$retirementAge, "retirementAge", input$currentAge, "current age")
    )
    if (!allValid) {
      shinyFeedback::showToast(
        "error",
        "Please check your selections and try again"
      )
    }
    return(allValid)
  }
  
  repairInputsForRetirement = function() {
    allValid = all(
      validateAtLeast(input$expenses, "expenses", 0),
      validateAtLeast(input$additionalIncome, "additionalIncome", 0),
      validateAge(input$retirementAge, "retirementAge"),
      validateAgeAtLeast(input$additionalIncomeAge, "additionalIncomeAge", input$retirementAge, "retirement age"),
      validateAgeGreaterThan(input$lifeExpectancy, "lifeExpectancy", input$retirementAge, "retirement age")
    )
    if (!allValid) {
      shinyFeedback::showToast(
        "error",
        "Please check your selections and try again"
      )
    }
    return(allValid)
  }
  
  observeEvent(simulateClick(), {
    req(repairInputsForContributions(), input$initialInvestments, input$contributions, input$investingGrowth)
    disableSimulations()
    fundsAtStart = rep.int(input$initialInvestments, n)
    
    simulateEachFund(
      fundsAtStart,
      input$contributions,
      length(monthsBetweenYears(input$currentAge, input$retirementAge)),
      input$investingGrowth
    ) %>%
      mutate(time = input$currentAge + (time / 12)) %>%
      simulatedInvestments()
    
    enableSimulations()
  })
  
  observeEvent(simulateRetirementClick(), {
    req(repairInputsForRetirement(), simulatedInvestments(), input$expenses, input$lifeExpectancy, input$retirementGrowth)
    disableSimulations()
    fundsAtRetirement = simulatedInvestments() %>%
      filter(time == max(time)) %>%
      select(value) %>%
      t() %>%
      as.vector()
    
    # assume there is only one phases
    simOneLength = (input$lifeExpectancy - input$retirementAge) * 12
    # check if there are two phases
    if (input$lifeExpectancy > input$additionalIncomeAge &&
        input$additionalIncomeAge > input$retirementAge) {
      simOneLength = (input$additionalIncomeAge - input$retirementAge) * 12
    }
    
    if (input$additionalIncomeAge == input$retirementAge) {
      expenses = -input$expenses + input$additionalIncome
    } else {
      expenses = -input$expenses
    }
    
    # simulate first phase of retirement 
    r = simulateEachFund(
      fundsAtRetirement,
      expenses,
      simOneLength,
      input$retirementGrowth
    ) %>%
      mutate(time = input$retirementAge * 12 + time)
    
    # simulate second phase of retirement, if necessary
    if (input$lifeExpectancy > input$additionalIncomeAge &&
        input$additionalIncomeAge > input$retirementAge) {
      fundsBeforeSupplement = r %>%
        filter(time == max(time)) %>%
        select(value) %>%
        t() %>%
        as.vector()
      s = simulateEachFund(
        fundsBeforeSupplement,
        -input$expenses + input$additionalIncome,
        (input$lifeExpectancy - input$additionalIncomeAge) * 12 - 1,
        input$retirementGrowth
      ) %>%
        mutate(time = input$additionalIncomeAge * 12 + time + 1)
      r = bind_rows(r, s)
    }
    
    r %>%
      mutate(time = time / 12) %>%
      simulatedRetirement()
    
    enableSimulations()
  })
  
  output$investmentsAtRetirement = function() {
    req(simulatedInvestments())
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
  
  output$investmentGrowth = renderDygraph({
    req(simulatedInvestments())
    isolate({
      end = simulatedInvestments() %>%
        filter(time == max(time)) %>%
        summarize(median = median(value), max = max(value))
      simulations = simulatedInvestments() %>%
        tidyr::spread(key = variable, value = value)
      yRange = c(0, max(1.2 * input$initialInvestments, min(1.1 * end$max, 2 * end$median)))
    })
    investmentGraph(simulations, yRange, "Simulations of Investment Growth Prior To Retirement")
  })
  
  output$retirement = renderDygraph({
    req(simulatedRetirement())
    isolate({
      start = simulatedRetirement() %>%
        filter(time == min(time)) %>%
        summarize(max = max(value))
      end = simulatedRetirement() %>%
        filter(time == max(time)) %>%
        summarize(median = median(value))
      simulations = simulatedRetirement() %>%
        tidyr::spread(key = variable, value = value)
      yRange = c(0, max(1.5 * start$max, 2 * end$median))
    })
    investmentGraph(simulations, yRange, "Simulations of Draw Down During Retirement")
  })
  
  output$epitaph = function() {
    req(simulatedRetirement())
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
