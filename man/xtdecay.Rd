\name{xtdecay}
\alias{xtdecay}
\title{Linear decay}
\source{
  Zachary M. Jones, "Some Time-Series Functions for Panels
  with Missingness",
  \url{http://www.zmjones.com/panel-ts.html}
}
\usage{
  xtdecay(data, x, cutpoint)
}
\arguments{
  \item{data}{a data frame with the \code{\link{xtdata}}
  attribute.}

  \item{x}{the variable for which to compute time since
  event.}

  \item{cutpoint}{the decay cut-point.}
}
\description{
  Linear decay function adapted from the
  \code{\link[doBy]{doBy}} package by Zachary M. Jones, and
  modified to understand the \code{\link{xtdata}}
  attribute.
}
\author{
  Zachary M. Jones
}
\seealso{
  \code{\link[doBy]{doBy}}
}
\keyword{events}
\keyword{xt}

