##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Return the number of scenarios of a model

nscenario <- function(model) {
  nscenario <- NA
  if(model$scenarios > 0) { nscenario <- model$scenarios }
  return(nscenario)
}