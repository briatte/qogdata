\name{xtmerge}
\alias{xtmerge}
\title{Merge \code{xtdata} data frames}
\usage{
  xtmerge(x, y, t = "year", t.x = NULL, t.y = NULL, ...)
}
\arguments{
  \item{x,}{y data frames with the \code{\link{xtdata}}
  attribute. IF the \code{type} parameter must be set to
  \code{"country"}, the function will try to resolve data
  frames with different country codes by matching them to
  \code{iso3n} codes with the \code{\link{countrycode}}
  package.}

  \item{t,}{t.x, t.y the time variable(s) by their names in
  the data frames. Defaults to \code{"year"}, which
  propagates to \code{t.x} and \code{t.y}.}

  \item{...}{other methods passed to \code{\link{merge}}}
}
\description{
  This function merges panel data based on their
  \code{"xtdata"} attributes.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Load UDS democracy scores.
UDS = get_uds()
# Merge QOG and UDS time series.
xt(xtmerge(qog.ts.demo, UDS))
names(xtmerge(qog.ts.demo, UDS))
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{merge}}
}

