\name{xtmerge}
\alias{xtmerge}
\title{Merge \code{xtdata} data frames}
\usage{
  xtmerge(x, y, t = "year", t.x = NULL, t.y = NULL, ...)
}
\arguments{
  \item{x}{a data frame with the \code{\link{xtdata}}
  attribute. See 'Details'.}

  \item{y}{a data frame with the \code{\link{xtdata}}
  attribute. See 'Details'.}

  \item{t}{the name of the time period variable in the data
  frames, which propagates to \code{t.x} and \code{t.y}.
  Defaults to \code{"year"}.}

  \item{t.x}{the name of the time period variable in the
  first dataset.}

  \item{t.y}{the name of the time period variable in the
  second dataset.}

  \item{...}{other methods passed to \code{\link{merge}},
  typically instructions on whether to perform an inner or
  outer merge; \code{xtmerge} defaults, like \code{merge},
  to an inner merge.}
}
\value{
  a data frame
}
\description{
  This function merges panel data based on their
  \code{"xtdata"} attributes.
}
\details{
  The function is intended to work as \code{merge} with a
  safety check: it will refuse to merge data that do not
  carry identical formats for their unique identifiers and
  time periods, as it will refuse to merge data of
  different primary \code{type}.

  If the \code{type} parameter is set to \code{"country"},
  the function will also try to resolve data frames with
  different country code formats by matching them to
  \code{iso3n} codes with \code{\link{xtcountry}}.
}
\examples{
if(require(countrycode)) {
  # Load QOG demo datasets.
  data(qog.demo)
  # Load UDS democracy scores.
  UDS = get_uds()
  # Merge QOG and UDS time series.
  xt(xtmerge(qog.ts.demo, UDS))
  names(xtmerge(qog.ts.demo, UDS))
}
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtcountry}}, \code{\link{xtdata}},
  \code{\link{merge}}
}
\keyword{xt}

