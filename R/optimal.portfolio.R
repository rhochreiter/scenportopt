##### Scenario-based Portfolio Optimization (scenportopt)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.finance-r.com/

# Meta-function to optimize portfolios

optimal.portfolio <- function(input=NULL) {
  # check whether the passed parameter is a portfolio.model
  # if not, create a default model
  if('portfolio.model' %in% class(input)) {
    model <- input
  } else {
    # add: check for (length(dim(data)) == 2) and colnames
    model <- portfolio.model(input)
  }

  # select appropriate optimization function
  if (model$objective == "markowitz") {
    model <- optimal.portfolio.markowitz(model) 
  }
  if (model$objective == "mad") {
    if (model$active.extension) {
      model <- optimal.portfolio.mad.long.short(model)
    } else {  
      model <- optimal.portfolio.mad(model)
    }
  }
  if (model$objective == "expected.shortfall") {
    if (model$active.extension) {
      model <- optimal.portfolio.expected.shortfall.long.short(model)
    } else {
      model <- optimal.portfolio.expected.shortfall(model)
    }
  }
  
  # return model
  return(model)
}