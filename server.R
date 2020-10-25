
library(shiny)
library(shinyjs)
library(dygraphs)
library(htmlwidgets)
library(lubridate)

server <- function(input, output) {
  
  monthsToRetire = reactive({
    req(input$endDate > input$startDate)
    seq(from = input$startDate, to = input$endDate, by = 'month')
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
  
  observeEvent(simulateClick(), {
    req(input$initialInvestments, input$investments, monthsToRetire(), input$growth)
    disableSimulations()
    fundsAtStart = rep.int(input$initialInvestments, n)
    
    simulateEachFund(
      fundsAtStart,
      input$investments,
      length(monthsToRetire()),
      input$growth
    ) %>%
      simulatedInvestments()
    
    enableSimulations()
  })
  
  observeEvent(simulateRetirementClick(), {
    req(simulatedInvestments(), input$expenses, input$retirementLength)
    disableSimulations()
    fundsAtRetirement = simulatedInvestments() %>%
      filter(time == max(time)) %>%
      select(value) %>%
      t() %>%
      as.vector()
    
    simulateEachFund(
      fundsAtRetirement,
      -input$expenses,
      input$retirementLength * 12,
      input$growth) %>%
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
    end = simulatedInvestments() %>%
      filter(time == max(time)) %>%
      summarize(median = median(value))
    s = simulatedInvestments() %>%
      tidyr::spread(key = variable, value = value) %>%
      select(-time)
    
    t = ts(frequency = 12, start = c(year(input$startDate), month(input$startDate)), data = s)
    dygraph(t, main = "Investment Growth Prior To Retirement") %>%
      dyAxis("y", label = "Value",
             valueRange = c(0, 2 * end$median), axisLabelWidth=80,
             axisLabelFormatter=htmlwidgets::JS(FUNC_JSFormatNumber),
             valueFormatter=htmlwidgets::JS(FUNC_JSFormatNumber)) %>%
      dyAxis("x", label = "Time", drawGrid = F) %>%
      dyLegend(show = "never") %>%
      dyOptions(colors = c("#0077b6","#593f62","#fe5f55", "#18f2b2", "#002A22",
                           "#fcbf49", "#F6F740", "#04724D", "#A67DB8", "#A40E4C")) %>%
      dyHighlight(highlightCircleSize = 1,
                  highlightSeriesBackgroundAlpha = 0.3)
  })
  
  output$retirement = renderDygraph({
    req(simulatedRetirement())
    start = simulatedRetirement() %>%
      filter(time == min(time)) %>%
      summarize(median = median(value))
    end = simulatedRetirement() %>%
      filter(time == max(time)) %>%
      summarize(median = median(value))
    s = simulatedRetirement() %>%
      tidyr::spread(key = variable, value = value) %>%
      select(-time)
    
    t = ts(frequency = 12, start = c(year(input$endDate), month(input$endDate)), data = s)
    dygraph(t, main = "Draw down during retirement") %>%
      dyAxis("y", label = "Value",
             valueRange = c(0, max(2 * start$median, 3 * end$median)), axisLabelWidth=80,
             axisLabelFormatter=htmlwidgets::JS(FUNC_JSFormatNumber),
             valueFormatter=htmlwidgets::JS(FUNC_JSFormatNumber)) %>%
      dyAxis("x", label = "Time", drawGrid = F) %>%
      dyLegend(show = "never") %>%
      dyOptions(colors = c("#0077b6","#593f62","#fe5f55", "#18f2b2", "#002A22",
                           "#fcbf49", "#F6F740", "#04724D", "#A67DB8", "#A40E4C")) %>%
      dyHighlight(highlightCircleSize = 1,
                  highlightSeriesBackgroundAlpha = 0.3)
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
