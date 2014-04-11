##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# 1 over N portfolio

optimal.portfolio.1overN <- function(input) {
  if('portfolio.model' %in% class(input)) { 
    m <- input 
  } else { 
    m <- portfolio.model(input) 
    m$objective <- "1overN"
  }
  
  ### Add 1 over N portfolio to model  
  portfolio <- list()
  portfolio$x <- rep(1/nasset(m), nasset(m))
  portfolio$x <- round(portfolio$x, m$precision)  
  m$portfolio <- portfolio
  return(m) 
}