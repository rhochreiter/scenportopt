##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Compute an efficient frontier of one model

efficient.frontier <- function(model, nportfolio=20, eps=4) {
  models <- list()

  # determine whether the risk measure is appropriate for efficient frontiers
  if (model$objective %in% c("markowitz", "mad", "expected.shortfall")) {
          
    # compute the minimum risk portfolio
    model$min.mean <- NULL
    model$fix.mean <- NULL
    model.min.risk <- optimal.portfolio(model)
      
    # compute the maximum/minimum return possible
    ret_max <- mean(l(optimal.portfolio.reward(model, type="max")))
    ret_min <- mean(l(optimal.portfolio.reward(model, type="min")))

    frontier_from <- round(min(asset.apply(model, mean)), eps) + 10^-eps
    frontier_to <- round(max(asset.apply(model, mean)), eps) - 10^-eps

    for (fm in seq(frontier_from, frontier_to, length.out=nportfolio)) {
      model$fix.mean <- fm
      model$max.mean <- fm
      models[[length(models) + 1]] <- optimal.portfolio(model)
    }  
  }  
  
  efficient.frontier <- list()
  efficient.frontier$model <- model
  efficient.frontier$models <- models
  efficient.frontier$mean <- sapply(models, mean)
  efficient.frontier$risk <- sapply(models, function(x) { risk(x) })
  efficient.frontier$model.min.risk <- model.min.risk
  
  class(efficient.frontier) <- "efficient.frontier"
  return(efficient.frontier)
}
