\name{xtdes}
\alias{xtdes}
\title{Describe panel data}
\usage{
  xtdes(data)
}
\arguments{
  \item{data}{a data frame carrying an \code{xtdata}
  attribute}
}
\value{
  a vector
}
\description{
  Function to describe the unique identifier, time period
  and distribution of $T$ for a data frame carrying an
  \code{xtdata} attribute. The function is similar to the
  \code{xtdes} command in Stata.
}
\examples{
q = xtdes(qog.ts.demo)
summary(q)
}
\seealso{
  \code{\link{xtset}}
}
\keyword{xt}

