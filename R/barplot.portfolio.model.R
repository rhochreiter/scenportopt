##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Overload barplot() for portfolio.model

barplot.portfolio.model <- function(model) {
  labels <- names(model$data)[which(model$portfolio$x == 0)]
  labels[which(model$portfolio$x == 0)] <- NA
  barplot(las=3, model$portfolio$x[which(model$portfolio$x > 0)], names.arg=names(model$data)[which(model$portfolio$x > 0)], col=topo.colors(1)) 
}