##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

### Portfolio Optimization minimizing Standard Deviation
# Implementation based on [Markowitz 1952]

# minimize { t(x) * Cov(data) * x }

optimal.portfolio.markowitz <- function(input) {
  if('portfolio.model' %in% class(input)) { 
    m <- input 
  } else {
    m <- portfolio.model(input) 
  }
  
  ### Variables: x
  n_var <- m$assets
  ix_x <- 1
  
  ### Objective function
  Objective <- list()
  Objective$quadratic <- cov(m$data)
  Objective$linear <- rep(0, m$assets)
  
  ### Constraints
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)
  
  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c(1:m$assets), m$sum.portfolio)

  # sum(a) { x[a] * mean[a] } => min.mean
  if(!is.null(m$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c(1:m$assets), -m$min.mean, -1*m$asset.means) }

  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(m$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c(1:m$assets), m$fix.mean, m$asset.means) }
  
  ### Bounds
  Bounds <- list()
  if (length(m$asset.bound.lower) == 1) { Bounds$lower <- rep(m$asset.bound.lower, m$assets) } else { Bounds$lower <- m$asset.bound.lower }
  if (length(m$asset.bound.upper) == 1) { Bounds$upper <- rep(m$asset.bound.upper, m$assets) } else { Bounds$upper <- m$asset.bound.upper }

  ### Solve optimization problem using modopt.quadprog
  solution <- quadprog(Objective$quadratic, Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)

  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x
  portfolio$x <- round(portfolio$x, m$precision)  
  m$portfolio <- portfolio
  return(m) 
}