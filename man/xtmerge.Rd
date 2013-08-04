\name{xtmerge}
\alias{xtmerge}
\title{Merge \code{xtdata} data frames}
\usage{
  xtmerge(x, y, t = "year", t.x = NULL, t.y = NULL, ...)
}
\arguments{
  \item{x,}{y data frames with the \code{\link{xdata}}
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
## Merge QOG and UDS time series.
# xtdata(QOG <- qogdata(file = TRUE, version = "bas", format = "ts"))
# xtdata(UDS <- get_uds())
# df = xtmerge(QOG, UDS)
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{merge}}
}

