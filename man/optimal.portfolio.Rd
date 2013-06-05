\name{optimal.portfolio}
\alias{optimal.portfolio}
\title{Portfolio Optimization}
\description{Calculate an optimal portfolio given a scenario set}

\details{
Calculate an optimal portfolio given a scenario set
}

\usage{
  optimal.portfolio(scenario_set, model=NULL)
}

\arguments{
  \item{scenario_set}{Scenario Set}
  \item{model}{Optimization parameters}
}

\value{
  \item{portfolio}{Solution object}
}

\references{\url{http://www.finance-r.com/}}

\author{Ronald Hochreiter \email{ron@hochreiter.net}}

\examples{
  data(djia2012w)
  portfolio <- optimal.portfolio(data)
  cat(portfolio$x)
}

\keyword{Portfolio Optimization}