##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# return respective risk of each asset

asset.risk <- function(model) {
  asset.risk <- NA
  if (model$objective == "markowitz") { asset.risk <- asset.apply(model, sd) }
  if (model$objective == "mad") { asset.risk <- asset.apply(model, mad) }
  if (model$objective == "expected.shortfall") { asset.risk <- asset.apply(model, expected.shortfall, model$alpha) }
  return(as.vector(asset.risk))
}
