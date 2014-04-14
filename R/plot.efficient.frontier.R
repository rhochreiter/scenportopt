##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# plot an efficient frontier

plot.efficient.frontier <- function(frontier, 
#                                    plot.min.risk.portfolio=TRUE, 
                                    ...) {  

  # xlim and ylim (depending on min risk or max risk)
  xlim <- c(min(frontier$risk), max(asset.risk(frontier$model)))
  ylim <- c(min(asset.apply(frontier$model, mean)), max(asset.apply(frontier$model, mean)))
  if(frontier$model$objective %in% c("expected.shortfall")) {
    xlim <- c(min(asset.risk(frontier$model)), max(frontier$risk))
    ylim <- c(min(asset.apply(frontier$model, mean)), max(asset.apply(frontier$model, mean)))
  }
  
  # plot frontier
  plot(frontier$risk, frontier$mean, type="l", xlab="Risk", ylab="Return", main="Efficient Frontier", 
       xlim=xlim, ylim=ylim, lwd=2, ...)

  points(frontier$risk, frontier$mean, pch=20)
  
  # plot assets
  points(asset.risk(frontier$model.min.risk), asset.apply(frontier$model.min.risk, mean))
  
  # plot minimum risk portfolio
#  if (plot.min.risk.portfolio) {
    points(risk(frontier$model.min.risk), mean(frontier$model.min.risk), col="red", pch=16)
#  }

}