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
\description{
  Function to check for panel data properties in a data
  frame. The syntax is provided in the 'Details' of
  \code{\link{xtset}}. Intended for programming purposes.
}
\examples{
## Set and check the QOG Basic time series data (not run).
# QOG = qogdata(file = TRUE, version = "bas", format = "ts")
# QOG = xtset(QOG)
# xtdata(QOG)
}
\seealso{
  \code{\link{xtset}}
}

