##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Compute an efficient frontier of a loss function

cvar <- avar <- es <- expected.shortfall <- function(loss, alpha=NA) {
  sorted.loss <- sort(loss)
  if (is.na(alpha)) { alpha <- model$alpha }
  value <- mean(sorted.loss[1:round(alpha * nscenario(model))])
  return(value)
}
