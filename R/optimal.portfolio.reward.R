##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

### compute maximum/minimum return portfolio given the constraints

optimal.portfolio.reward <- function(input, type="max") {
  if('portfolio.model' %in% class(input)) { 
    model <- input 
  } else { 
    model <- portfolio.model(input) 
    model$objective <- "return"
  }
  
  ### Variables: x
  n_var <- model$assets
  ix_x <- 1
  
  ### Objective function
  Objective <- list()
  Objective$linear <- -model$asset.means
  if (type=="min") { Objective$linear <- model$asset.means }
  
  ### Constraints
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)

  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c(ix_x:(ix_x+model$assets-1)), model$sum.portfolio)
  
  # sum(a) { x[a] * mean[a] } => min.mean
  if(!is.null(model$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c(1:model$assets), -model$min.mean, -1*model$asset.means) }
  
  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(model$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c(1:model$assets), model$fix.mean, model$asset.means) }

  ### Bounds
  Bounds <- list()
  
  # Portfolio constrained to model parameters
  Bounds$lower[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.lower
  Bounds$upper[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.upper
  
  ### Solve optimization problem using modopt.linprog  
  solution <- linprog(Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)
  
  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+model$assets-1)]
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model)
}