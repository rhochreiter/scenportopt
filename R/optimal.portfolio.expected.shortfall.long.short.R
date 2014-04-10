##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

### Portfolio Optimization minimizing Conditional Value at Risk (CVaR)
# Implementation based on [Rockafellar and Uryasev 2001]

# maximize { b - sum(s) { ( p[s] * z[s] ) / alpha } }
# for(s) { loss[s] == sum(a) { (data[s,a] * x[a]) } }
# for(s) { b - loss[s] - z[s] <= 0 } // z >= b - loss
# for(s) { z[s] >= 0 }

optimal.portfolio.expected.shortfall.long.short <- function(input) {
  if('portfolio.model' %in% class(input)) { 
    m <- input 
  } else { 
    m <- portfolio.model(input) 
    m$objective <- "expected.shortfall.long.short"
  }
  
  ### Variables: b, x, xp, xm, loss, z
  n_var <- 1 + 3*m$assets + 2*m$scenarios
  ix_b <- 1
  ix_x <- 2
  ix_xp <- ix_x + m$assets
  ix_xm <- ix_xp + m$assets
  ix_loss <- ix_xm + m$assets
  ix_z <- ix_loss + m$scenarios

  ### Objective function
  
  # maximize { b - sum(s) { ( p[s] * z[s] ) / alpha } }
  Objective <- list()
  Objective$linear <- rep(0, n_var)
  Objective$linear[ix_b] <- 1
  for (s in 0:(m$scenarios-1)) { Objective$linear[ix_z+s] <- -m$scenario.probabilities[s+1]/m$alpha }
  
  ### Constraints
  Constraints <- list(n=n_var, A=NULL, b=NULL, Aeq=NULL, beq=NULL)
 
  # sum(a) { x[a] } == sum.portfolio
  Constraints <- linear.constraint.eq(Constraints, c(ix_x:(ix_x+m$assets-1)), m$sum.portfolio)
  
  # sum(a) { x[a] * mean[a] } => min.mean
  if(!is.null(m$min.mean)) { Constraints <- linear.constraint.iq(Constraints, c(1:m$assets), -m$min.mean, -1*m$asset.means) }
  
  # sum(a) { x[a] * mean[a] } == fix.mean
  if(!is.null(m$fix.mean)) { Constraints <- linear.constraint.eq(Constraints, c(1:m$assets), m$fix.mean, m$asset.means) }

  ### Long/Short constraints
  
  # sum(a) { xp[a] } == sum.long
  Constraints <- linear.constraint.eq(Constraints, c(ix_xp:(ix_xp+m$assets-1)), m$sum.long)

  # sum(a) { xm[a] } == sum.short
  Constraints <- linear.constraint.eq(Constraints, c(ix_xm:(ix_xm+m$assets-1)), m$sum.short)
  
  # for(a) { xp[a] - xm[a] - x[a] == 0 } // portfolio == xp - xm
  for (a in 0:(m$assets-1)) { Constraints <- linear.constraint.eq(Constraints, c(ix_x+a, ix_xp+a, ix_xm+a), 0,  c(-1, 1, -1)) }
  
  ### CVaR constraints
  
  # for(s) { loss[s] == sum(a) { (data[s,a] * x[a]) } }
  for (s in 0:(m$scenarios-1)) { Constraints <- linear.constraint.eq(Constraints, c((ix_x:(ix_x+m$assets-1)), ix_loss+s), 0, c(as.vector(m$data[(s+1),]), -1)) }
  
  # for(s) { b - loss[s] - z[s] <= 0 } // z >= b - loss
  for (s in 0:(m$scenarios-1)) { Constraints <- linear.constraint.iq(Constraints, c(ix_b, ix_loss+s, ix_z+s), 0, c(1,-1,-1)) }
  
  ### Bounds
  Bounds <- list()
  
  # all variables unbounded
  M <- 1e9
  Bounds$lower <- rep(-M, n_var)
  Bounds$upper <- rep(M, n_var)
  
  # portfolio constrained to model parameters
  Bounds$lower[(ix_x):(ix_x+m$assets-1)] <- m$asset.bound.lower
  Bounds$upper[(ix_x):(ix_x+m$assets-1)] <- m$asset.bound.upper
  
  # xp >= 0;
  Bounds$lower[(ix_xp):(ix_xp+m$assets-1)] <- 0
  
  # xm >= 0;
  Bounds$lower[(ix_xm):(ix_xm+m$assets-1)] <- 0
  
  # z >= 0
  Bounds$lower[(ix_z):(ix_z+m$scenarios-1)] <- 0
    
  ### Solve optimization problem using modopt.linprog  
  solution <- linprog(-Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)
  
  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+m$assets-1)]
  portfolio$x <- round(portfolio$x, m$precision)  
  m$portfolio <- portfolio
  return(m)
}