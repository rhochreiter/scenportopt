optimal.portfolio.expected.shortfall <- function(scenario_set, model=NULL) {
  
  ### variables
  
  # b, x, loss, z
  n_var <- 1 + model$assets + 2*model$scenarios
  ix_b <- 1
  ix_x <- 2
  ix_loss <- ix_x + model$assets
  ix_z <- ix_loss + model$scenarios
  
  ### constraints
  
  # sum(portfolio) == 1
  add_a <- rep(0, n_var)
  add_a[(ix_x):(ix_x+model$assets-1)] <- 1
  if (exists("Aeq")) { Aeq <- rbind(Aeq, add_a) } else { Aeq <- add_a }
  if (exists("beq")) { beq <- c(beq, 1) } else { beq <- 1 }
  
  # loss == (scenario_set * portfolio)
  for (s in 0:(model$scenarios-1)) {
    add_a <- rep(0, n_var)
    add_a[ix_x:(ix_x+model$assets-1)] <- data[(s+1),]
    add_a[ix_loss+s] <- -1
    if (exists("Aeq")) { Aeq <- rbind(Aeq, add_a) } else { Aeq <- add_a }
    if (exists("beq")) { beq <- c(beq, 0) } else { beq <- 0 }    
  }
  
  # z >= b - loss
  # b - loss[s] - z[s] <= 0
  for (s in 0:(model$scenarios-1)) {
    add_a <- rep(0, n_var)
    add_a[ix_b] <- 1
    add_a[ix_loss+s] <- 1
    add_a[ix_loss+s] <- -1
    add_a[ix_z+s] <- -1
    if (exists("A")) { A <- rbind(A, add_a) } else { A <- add_a }
    if (exists("b")) { b <- c(b, 0) } else { b <- 0 }
  }
  
  ### lower and upper bounds
  M <- 1e9
  lb <- rep(-M, n_var)
  ub <- rep(M, n_var)
  
  # x
  lb[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.lower
  ub[(ix_x):(ix_x+model$assets-1)] <- model$asset.bound.upper
  
  # z >= 0
  lb[(ix_z):(ix_z+model$scenarios-1)] <- 0
  
  ### objective
  
  #maximize( b - ( ( sum(probability * z) ) / alpha ) )
  obj <- rep(0, n_var)
  obj[ix_b] <- 1
  for (s in 0:(model$scenarios-1)) { obj[ix_z+s] <- -model$scenario.probabilities[s+1]/model$alpha }
  
  ### solve using modopt.linprog  
  solution <- linprog(-obj, A, b, Aeq, beq, lb, ub)
  
  ### portfolio  
  portfolio <- list()
  portfolio$x <- solution$x[ix_x:(ix_x+model$assets-1)]
  
  return(portfolio)
}