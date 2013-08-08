\name{xt}
\alias{xt}
\title{Get panel data properties}
\usage{
  xt(dataset)
}
\arguments{
  \item{dataset}{the dataset from which to return the
  \code{xtdata} attributes.}
}
\value{
  a list of \code{xtdata} attributes, if it exists.
}
\description{
  Function to extract the panel data properties of a data
  frame. The syntax is provided in the 'Details' of
  \code{\link{xtset}}. Intended for programming purposes.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Identify the QOG Basic time series data properties.
xt(qog.ts.demo)
}
\seealso{
  \code{\link{xtset}}
}
\keyword{internal}
\keyword{xt}

