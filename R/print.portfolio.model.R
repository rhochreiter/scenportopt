##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Overload print() for objects of S3 class portfolio.model

print.portfolio.model <- function(model) {
  # currently only basic information
  cat("# Portfolio model\n")  
  cat("- Objective:", model$objective, "\n")
  cat("- Scenarios:", model$scenarios, "\n")
  cat("- Assets:", model$assets, "\n")
}