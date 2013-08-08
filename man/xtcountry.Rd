\name{xtcountry}
\alias{xtcountry}
\title{Try ISO-3N country code conversion on \code{xtdata} data frames}
\usage{
  xtcountry(dataset)
}
\arguments{
  \item{dataset}{a data frame with the \code{\link{xtdata}}
  attribute. The \code{type} parameter must be set to
  \code{"country"}.}
}
\value{
  a vector of how many observations were successfully
  matched on their country code, short country name and
  long country name, based on the variable names specified
  as \code{\link{xtdata}} properties.
}
\description{
  This function tests conversions to ISO3-N country codes
  on the country codes, acronyms and names identified in a
  data frame that carries the \code{\link{xtdata}}
  attribute with the \code{country} type. Used by
  \code{\link{xtmerge}}.
}
\examples{
if(require(countrycode)) {
  # Test the country identifiers in the QOG dataset.
  data(qog.demo)
  xtcountry(qog.ts.demo)
}
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{xtmerge}}
}
\keyword{country}
\keyword{internal}
\keyword{xt}

