optimal.portfolio <- function(scenario_set, model=NULL) {
  if(is.null(model)) { model <- list() }
  model <- model.check(model)
  model <- model.scenario.check(model, scenario_set)  
  
  if (model$objective == "markowitz") { portfolio <- optimal.portfolio.markowitz(scenario_set, model) }
  if (model$objective == "expected.shortfall") { portfolio <- optimal.portfolio.expected.shortfall(scenario_set, model) }
  if (model$objective == "mad") { portfolio <- optimal.portfolio.mad(scenario_set, model) }    
  
  portfolio$model <- model
  
  return(portfolio)
}