model.check <- function(model) {
  if (is.null(model$objective)) { model$objective <- "markowitz" }
  if (is.null(model$asset.bound.lower)) { model$asset.bound.lower <- 0 }
  if (is.null(model$asset.bound.upper)) { model$asset.bound.upper <- 1 }
  if (is.null(model$alpha)) { model$alpha <- 0.05 }  
  
  if (model$objective == "avar") { model$objective = "expected.shortfall" }
  if (model$objective == "cvar") { model$objective = "expected.shortfall" }
  class(model) <- "portfolio.model"
  
  return(model)
}