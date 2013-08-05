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
\description{
  This function tests conversions to ISO3-N country codes
  on the country codes, acronyms and names identified in a
  data frame that carries the \code{\link{xtdata}}
  attribute with the \code{country} type. Used by
  \code{\link{xtmerge}}.
}
\examples{
## Test the country identifiers in the QOG dataset (not run).
# data(qog.demo)
# xtcountry(qog.ts.demo)
## Test the country identifiers in the UDS dataset (not run).
# UDS = get_uds()
# xtcountry(UDS)
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{xtmerge}}
}

