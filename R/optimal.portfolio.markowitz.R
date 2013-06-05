optimal.portfolio.markowitz <- function(scenario_set, model=NULL, prec=8) {
  H <- cov(scenario_set)
  f <- rep(0, model$assets)
  Aeq <- rep(1, model$assets)
  beq <- 1
  lb <- rep(model$asset.bound.lower, model$assets)
  ub <- rep(model$asset.bound.upper, model$assets)
  
  solution <- quadprog(H, f, NULL, NULL, Aeq, beq, lb, ub)
  portfolio <- list()
  portfolio$x <- solution$x
  
  portfolio$x <- round(portfolio$x, prec)
  
  return(portfolio)  
}