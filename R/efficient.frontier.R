##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Compute an efficient frontier of one model

efficient.frontier <- function(model, nportfolio=20, eps=4) {
    models <- list()

    asset.mean <- apply(model$data, 2, mean)
    asset.sd <- apply(model$data, 2, sd)
    
    frontier_from <- round(min(asset.mean), eps) + 10^-eps
    frontier_to <- round(max(asset.mean), eps) - 10^-eps

    for (fm in seq(frontier_from, frontier_to, length.out=nportfolio)) {
      model$fix.mean <- fm 
      models[[length(models) + 1]] <- optimal.portfolio(model)
    }
    
    class(models) <- "efficient.frontier"
    return(models)
}