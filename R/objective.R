##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# check and set new objective function

objective <- function(model, objective) {
  # check if selected objective is supported
  supported_objectives <- list("reward", "1overN", "momentum", "markowitz", "sd", "standard.deviation", "variance", "mad", "cvar", "avar", "expected.shortfall")
  if (objective %in% supported_objectives == FALSE) {
    warning("Selected objective is not supported! Falling back to objective: markowitz")
    objective <- "markowitz"
  }
  
  # alias handling
  if (objective == "sd") { objective <- "markowitz" }
  if (objective == "standard.deviation") { objective <- "markowitz" }
  if (objective == "variance") { objective <- "markowitz" }
  if (objective == "cvar") { objective <- "expected.shortfall" }
  if (objective == "avar") { objective <- "expected.shortfall" }

  # set objective and return model
  model$objective <- objective
  return(model)
}
