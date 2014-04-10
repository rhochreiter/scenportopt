##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Return the loss distribution of the model

portfolio.loss <- l <- function(model) {
  if(is.na(model$portfolio)) { return(NA) }
  return(as.vector(model$portfolio$x %*% t(model$data)))
}