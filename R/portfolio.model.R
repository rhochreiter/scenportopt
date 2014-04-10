##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Create a portfolio.model instance (or fix an existing one)

portfolio.model <- function(scenario_set) {
  # initialize a new portfolio.model
  model <- list()
  
  # default values - model
  if (is.null(model$objective)) { model$objective <- "markowitz" }
  if (is.null(model$sum.portfolio)) { model$sum.portfolio <- 1 }
  if (is.null(model$asset.bound.lower)) { model$asset.bound.lower <- 0 }
  if (is.null(model$asset.bound.upper)) { model$asset.bound.upper <- 1 }
  if (is.null(model$alpha)) { model$alpha <- 0.05 }  
  if (is.null(model$precision)) { model$precision <- 8 }
  model$min.mean <- NULL
  model$fix.mean <- NULL
  model$sum.long <- NULL
  model$sum.short <- NULL
  if (is.null(model$active.extension)) { model$active.extension <- FALSE }

  # default values - scenario
  model$data <- scenario_set
  model$assets <- dim(scenario_set)[2]
  model$scenarios <- dim(scenario_set)[1]
  model$scenario.probabilities <- rep(1/model$scenarios, model$scenarios)
  model$asset.means <- as.vector(apply(scenario_set, 2, mean))

  # default values - portfolio
  model$portfolio <- NA
  
  # return S3 class instance of type portfolio.model
  class(model) <- "portfolio.model"  
  return(model)
}