model.scenario.check <- function(model, scenario_set) {
  model$assets <- dim(scenario_set)[2]
  model$scenarios <- dim(scenario_set)[1]
  if (is.null(model$scenario.probabilities)) { model$scenario.probabilities <- rep(1/model$scenarios, model$scenarios) }  
  return(model)
}