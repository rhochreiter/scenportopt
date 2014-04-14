##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# momentum portfolio

optimal.portfolio.momentum <- function(input) {
  if('portfolio.model' %in% class(input)) { 
    model <- input 
  } else { 
    model <- portfolio.model(input) 
    model$objective <- "momentum"
  }
  
  ### Find momentum assets
  asset.mean <- asset.apply(model, mean)
  sorted.asset.mean.ix <- sort(asset.mean, decreasing=TRUE, index.return=TRUE)$ix

  weights <- rep(0, nasset(model))
  # active extension
  if (model$active.extension) {
    if (is.null(model$momentum.long)) { 
      n.long <- ceiling(nasset(model) * 0.1)
    } else { n.long <- model$momentum.long }
    if (is.null(model$momentum.short)) { 
      n.short <- n.long 
    } else { n.short <- model$momentum.short }
    weights[sorted.asset.mean.ix[1:n.long]] <- model$sum.long/n.long 
    weights[sorted.asset.mean.ix[(nasset(model)-n.short+1):nasset(model)]] <- -model$sum.short/n.short
    
  # no active extension
  } else {  
    if (is.null(model$momentum.long)) { 
      n.long <- ceiling(nasset(model) * 0.1) 
    } else {
      n.long <- model$momentum.long
    }
    weights[sorted.asset.mean.ix[1:n.long]] <- model$sum.portfolio/n.long 
  }
  
  ### Add momentum portfolio to model  
  portfolio <- list()
  portfolio$x <- weights
  portfolio$x <- round(portfolio$x, model$precision)  
  model$portfolio <- portfolio
  return(model) 
}