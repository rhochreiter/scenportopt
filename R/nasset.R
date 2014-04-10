##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Return the number of assets of a model

nasset <- function(model) {
  nasset <- NA
  if(model$assets > 0) { nasset <- model$assets }
  return(nasset)
}