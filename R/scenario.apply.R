##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# scenario.apply() convenience function

scenario.apply <- function(model, ...) {
  value <- apply(model$data, 1, ...)
  return(value)
}
