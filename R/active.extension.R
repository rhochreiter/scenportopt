##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Add corresponding long/short constraints for a diverse set of
# active extension portfolios (e.g. 130/30 portfolios)

active.extension <- function(model, up, down) {
  if(up > 1) { up <- up/100 }
  if(down > 1) { down <- down/100 }

  model$sum.long <- up # 130/30: 1.3 
  model$sum.short <- down # 130/30: 0.3
  model$asset.bound.lower <- -down # 130/30: -0.3
  model$asset.bound.upper <- up # 130/30: 1.3
  
  model$active.extension <- TRUE
  
  return(model)
}
