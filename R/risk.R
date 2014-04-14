##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# return risk of model

risk <- function(model) {
  risk <- NA
  if (model$objective == "markowitz") { risk <- sd(l(model)) }
  if (model$objective == "mad") { risk <- mad(l(model)) }
  if (model$objective == "expected.shortfall") { risk <- expected.shortfall(l(model), model$alpha) }
  return(as.numeric(risk))
}
