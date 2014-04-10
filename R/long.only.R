##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Switch back to long only - useful after using active.extension()

long.only <- function(model) {  
  model$asset.bound.lower <- 0
  model$asset.bound.upper <- 1
  model$sum.portfolio <- 1
  model$sum.long <- NULL
  model$sum.short <- NULL
  
  model$active.extension <- FALSE
  
  return(model)
}
