##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# check and set new alpha

alpha <- function(model, alpha) {
  # check alpha
  if ((alpha < 0) | (alpha > 1)) { 
    warning("Invalid alpha chosen, switching back to 95%")
    alpha <- 0.05 
  }
  
  # set alpha and return model
  model$alpha <- alpha
  return(model)
}
