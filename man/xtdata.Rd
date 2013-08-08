\name{xtdata}
\alias{xtdata}
\title{Panel data properties}
\usage{
  xtdata(dataset)
}
\arguments{
  \item{dataset}{the dataset in which to check for
  \code{xtdata} attributes.}
}
\value{
  an error if the data frame has no \code{xtdata}
  attributes, or \code{TRUE} if the data frame has an
  \code{xtdata} attribute that conforms to the syntax of
  \code{\link{xtset}}.
}
\description{
  Function to check for panel data properties in a data
  frame. The syntax is provided in the 'Details' of
  \code{\link{xtset}}. Intended for programming purposes.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Check xtdata attribute of QOG time series demo dataset.
xtdata(qog.ts.demo)
## Unsuccessful checks send back an error (not run).
# xtdata(qog.cs.demo)
}
\seealso{
  \code{\link{xtset}}
}
\keyword{internal}
\keyword{xt}

