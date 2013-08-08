\name{qogjoin}
\alias{qogjoin}
\title{Join historical and recent states in QOG Standard time series data}
\usage{
  qogjoin(data, country = NULL)
}
\arguments{
  \item{data}{a QOG Standard time series dataset, or any
  data frame with \code{cname} (country) and \code{year}
  information coded as in the QOG Standard time series
  dataset.}

  \item{country}{the country name to join data over.
  Requires the \code{cname} variable.}
}
\value{
  a data frame with country-year observations
}
\description{
  Function to plot maps of Quality of Government (QOG)
  data. Requires the \code{ggplot2} and \code{maps}
  packages.
}
\details{
  The function will try to find two series of country-year
  observations that both match the \code{country} argument.
  Within the QOG Standard time series dataset, this will
  match historical states like "France (-1962)" to modern
  states like "France (1963-)". The function will then
  create a new variable out of both series, joined at their
  separation years, and set its country code attributes to
  the most recent ones. See Appendix A of the \emph{QOG
  Standard Codebook} for details on historical states in
  the QOG Standard time series dataset.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
QOG = qog.ts.demo
QOG = qogjoin(QOG, "Ethiopia")
QOG = qogjoin(QOG, "France")
QOG = qogjoin(QOG, "Malaysia")
QOG = qogjoin(QOG, "Pakistan")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\references{
  Teorell, Jan, Nicholas Charron, Stefan Dahlberg, Soren
  Holmberg, Bo Rothstein, Petrus Sundin & Richard Svensson.
  2013. \emph{The Quality of Government Dataset}, version
  15May13. University of Gothenburg: The Quality of
  Government Institute, \url{http://www.qog.pol.gu.se}.
}
\keyword{qog}

