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
\description{
  Function to extract the panel data properties of a data
  frame. The syntax is provided in the 'Details' of
  \code{\link{xtset}}. Intended for programming purposes.
}
\examples{
## Identify the QOG Basic time series data (not run).
# QOG = qogdata(file = TRUE, version = "bas", format = "ts")
# QOG = xtset(QOG)
# xt(QOG)
}
\seealso{
  \code{\link{xtset}}
}

