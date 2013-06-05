optimal.portfolio.mad <- function(scenario_set, model=NULL) {
  
  ### variables
  
  # x, y
  n_var <- model$assets + model$scenarios
  ix_x <- 1
  ix_y <- ix_x + model$assets
  
  ### constraints
  
  # sum(portfolio) == 1
  add_a <- rep(0, n_var)
  add_a[(ix_x):(ix_x+model$assets-1)] <- 1
  if (exists("Aeq")) { Aeq <- rbind(Aeq, add_a) } else { Aeq <- add_a }
  if (exists("beq")) { beq <- c(beq, 1) } else { beq <- 1 }
  
  # calculate a
  rt <- as.vector(colMeans(as.matrix(data)))
  at <- as.matrix(data-rt)
  
  # y + sum( a * x ) >= 0
  for (s in 0:(model$scenarios-1)) {
    add_a <- rep(0, n_var)
    add_a[ix_x:(ix_x+model$assets-1)] <- -at[(s+1),]
    add_a[ix_y+s] <- -1
    if (exists("A")) { A <- rbind(A, add_a) } else { A <- add_a }
    if (exists("b")) { b <- c(b, 0) } else { b <- 0 }    
  }
  
  # y - sum( a * x ) >= 0
  for (s in 0:(model$scenarios-1)) {
    add_a <- rep(0, n_var)
    add_a[ix_x:(ix_x+model$assets-1)] <- at[(s+1),]
    add_a[ix_y+s] <- -1
    if (exists("A")) { A <- rbind(A, add_a) } else { A <- add_a }
    if (exists("b")) { b <- c(b, 0) } else { b <- 0 }    
  }
  
  ### objective
  
  # minimize sum(y)/card(y) 
  obj <- rep(0, n_var)
  obj[ix_y:(ix_y+model$scenarios-1)] <- 1/model$scenarios
  
  ### lower and upper bounds
  M <- 1e9
  lb <- rep(-M, n_var)
  ub <- rep(M, n_var)
  
  # x
  lb[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.lower
  ub[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.upper
  
  ### solve using modopt.linprog  
  solution <- linprog(obj, A, b, Aeq, beq, lb, ub)
  
  ### portfolio  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+model$assets-1)]
  
  return(portfolio)
}