##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Create a portfolio.model instance (or fix an existing one)

portfolio.model <- function(scenario_set) {
  # initialize a new portfolio.model
  model <- list()
  
  # default values - model
  model$objective <- "markowitz"
  model$precision <- 8
  model$active.extension <- FALSE
  model$sum.portfolio <- 1
  model$alpha <- 0.05
  model$min.mean <- NULL
  model$max.mean <- NULL
  model$fix.mean <- NULL
  model$sum.long <- NULL
  model$sum.short <- NULL
  model$momentum.long <- NULL
  model$momentum.short <- NULL
  
  # default values - scenario
  model$data <- scenario_set
  model$assets <- dim(scenario_set)[2]
  model$scenarios <- dim(scenario_set)[1]
  model$scenario.probabilities <- rep(1/model$scenarios, model$scenarios)
  model$asset.means <- as.vector(apply(scenario_set, 2, mean))
  
  # set upper bounds according to scenario set
  model$asset.bound.lower <- rep(0, model$assets)
  model$asset.bound.upper <- rep(1, model$assets)
  
  # default values - portfolio
  model$portfolio <- NA
  
  # return S3 class instance of type portfolio.model
  class(model) <- "portfolio.model"  
  return(model)
}