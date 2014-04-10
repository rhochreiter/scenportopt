asset.apply <- function(model, ...) {
  value <- apply(model$data, 2, ...)
  return(value)
}
  