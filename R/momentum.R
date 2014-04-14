##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# set momentum parameters

momentum <- function(model, nmomentum, nmomentum.short=NULL) {
  model$momentum.long <- nmomentum
  model$momentum.short <- nmomentum.short
  return(model)
}