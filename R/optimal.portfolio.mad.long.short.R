##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

### Portfolio Optimization minimizing Mean Absolute Deviation (MAD)
# Implementation based on [Konno and Yamazaki 1991]

# minimize { sum(s) { p[s] * y[s] } }
# for(s) { y[s] + sum(a) { a[s, a] * x[a] } >= 0 }
# for(s) { y[s] - sum(a) { a[s, a] * x[a] } >= 0 }

optimal.portfolio.mad.long.short <- function(input) {
  if('portfolio.model' %in% class(input)) { 
    m <- input 
  } else { 
    m <- portfolio.model(input) 
    m$objective <- "mad.long.short"
  }
  
  ### Variables: x, xp, xm, y
  n_var <- 3*m$assets + m$scenarios
  ix_x <- 1
  ix_xp <- ix_x + m$assets
  ix_xm <- ix_xp + m$assets
  ix_y <- ix_xm + m$assets
  
  ### Objective function
  
  # minimize { sum(s) { p[s] * y[s] } } // Konno/Yamazaki: minimize { sum(s) { y[s]/card(y) } }
  Objective <- list()
  Objective$linear <- rep(0, n_var)
  Objective$linear[ix_y:(ix_y+m$scenarios-1)] <- m$scenario.probabilities
  
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
  
  ### MAD constraints
  
  # calculate a[s, a]
  at <- as.matrix(m$data - as.vector(colMeans(as.matrix(m$data))))
  
  # for(s) { y[s] + sum(a) { a[s, a] * x[a] } >= 0 }
  for (s in 0:(m$scenarios-1)) { Constraints <- linear.constraint.iq(Constraints, c(ix_x:(ix_x+m$assets-1), ix_y+s), 0, c(-at[(s+1),], -1)) }
  
  # for(s) { y[s] - sum(a) { a[s, a] * x[a] } >= 0 }
  for (s in 0:(m$scenarios-1)) { Constraints <- linear.constraint.iq(Constraints, c(ix_x:(ix_x+m$assets-1), ix_y+s), 0, c(at[(s+1),], -1)) }
  
  ### Bounds
  Bounds <- list()
  
  # All variables unbounded
  M <- 1e9
  Bounds$lower <- rep(-M, n_var)
  Bounds$upper <- rep(M, n_var)
  
  # Portfolio constrained to model parameters
  Bounds$lower[(ix_x):(ix_x+m$assets-1)] <- m$asset.bound.lower
  Bounds$upper[(ix_x):(ix_x+m$assets-1)] <- m$asset.bound.upper

  # xp >= 0;
  Bounds$lower[(ix_xp):(ix_xp+m$assets-1)] <- 0
  
  # xm >= 0;
  Bounds$lower[(ix_xm):(ix_xm+m$assets-1)] <- 0
  
  ### Solve optimization problem using modopt.linprog  
  solution <- linprog(Objective$linear, Constraints$A, Constraints$b, Constraints$Aeq, Constraints$beq, Bounds$lower, Bounds$upper)
  
  ### Add optimal portfolio to model  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+m$assets-1)]
  portfolio$x <- round(portfolio$x, m$precision)  
  m$portfolio <- portfolio
  return(m) 
}