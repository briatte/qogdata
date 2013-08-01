\name{qogjoin}
\alias{qogjoin}
\title{Join historical and recent states in QOG Standard time series data}
\usage{
  qogjoin(data, country = NULL, out = "variable")
}
\arguments{
  \item{data}{a QOG Standard time series dataset, or any
  data frame with \code{cname} (country) and \code{year}
  information coded as in the QOG Standard time series
  dataset.}

  \item{country}{the country name to join data over.
  Requires the \code{cname} variable.}

  \item{out}{what to return. If set to \code{"data"}, the
  entire dataset is returned, stripped of the older
  observations and update to the new series. See
  'Details'.}
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
# Load QOG Standard time series data.
QOG = qogdata(tempfile(fileext = ".dta"), format = "ts",
              variables = c("cname", "wdi_gdp", "wdi_pop"))
# By default, the function returns the joined data.
head(qogjoin(QOG, "France"))
# Set out to "data" to get the modified dataset.
QOG <- qogjoin(QOG, "Pakistan", out = "data")
QOG <- qogjoin(QOG, "Ethiopia", out = "data")
QOG <- qogjoin(QOG, "Malaysia", out = "data")
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

