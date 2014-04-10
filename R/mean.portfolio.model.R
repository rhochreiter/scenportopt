##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Overload mean() for objects of S3 class portfolio.model

mean.portfolio.model <- function(m) {
  if(is.na(m$portfolio)) { return(NA) }
  return(mean(m$portfolio$x %*% t(data)))
}