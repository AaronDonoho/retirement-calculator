
library(dygraphs)
library(lubridate)

server <- function(session, input, output) {
  
  monthsToRetire = reactive({
    req(input$retirementAge > input$currentAge)
    seq(from = 12 * input$currentAge, to = 12 * input$retirementAge)
  })
  
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
  
  repairInputs = function() {
    allValid = T
    if (!validNumber(input$initialInvestments)) {
      updateNumericInput(session, "initialInvestments", value = 0)
      allValid = F
    }
    if (!validNumber(input$contributions)) {
      updateNumericInput(session, "contributions", value = 0)
      allValid = F
    }
    if (!validAge(input$currentAge)) {
      updateNumericInput(session, "currentAge", value = 35)
      allValid = F
    }
    if (!validAge(input$retirementAge)) {
      updateNumericInput(session, "retirementAge", value = 65)
      allValid = F
    }
    if (!validAge(input$lifeExpectancy)) {
      updateNumericInput(session, "lifeExpectancy", value = 100)
      allValid = F
    }
    if (!validNumber(input$expenses)) {
      updateNumericInput(session, "expenses", value = 0)
      allValid = F
    }
    return(allValid)
  }
  
  observeEvent(simulateClick(), {
    req(repairInputs(), input$initialInvestments, input$contributions, monthsToRetire(), input$investingGrowth)
    disableSimulations()
    fundsAtStart = rep.int(input$initialInvestments, n)
    
    simulateEachFund(
      fundsAtStart,
      input$contributions,
      length(monthsToRetire()),
      input$investingGrowth
    ) %>%
      mutate(time = input$currentAge + (time / 12)) %>%
      simulatedInvestments()
    
    enableSimulations()
  })
  
  observeEvent(simulateRetirementClick(), {
    req(repairInputs(), simulatedInvestments(), input$expenses, input$lifeExpectancy, input$retirementGrowth)
    disableSimulations()
    fundsAtRetirement = simulatedInvestments() %>%
      filter(time == max(time)) %>%
      select(value) %>%
      t() %>%
      as.vector()
    
    simulateEachFund(
      fundsAtRetirement,
      -input$expenses,
      (input$lifeExpectancy - input$retirementAge) * 12,
      input$retirementGrowth
    ) %>%
      mutate(time = input$retirementAge + (time / 12)) %>%
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
