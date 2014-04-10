##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Return the portfolio weights of the model

portfolio.weights <- x <- function(model) {
  if (is.null(model$portfolio)) { return(NULL) } 
  else { return(model$portfolio$x) }
}