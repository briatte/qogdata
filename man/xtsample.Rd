\name{xtsample}
\alias{xtsample}
\title{Sample out of an \code{\link{xtdata}} data frame}
\usage{
  xtsample(data, n = 20)
}
\arguments{
  \item{data}{data frame carrying an \code{\link{xtdata}}
  data frame}

  \item{n}{how many observations to sample}
}
\description{
  Function to extract a sample of observations out of a
  panel dataset, preserving all time measurements for each
  sampled observation.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Random sample of ten QOG countries.
unique(xtsample(qog.ts.demo, 10)$cname)
# Random cross-section of year 2000.
xtsubset(xtsample(qog.ts.demo, 10), year == 2000)[, 1:5]
}
\seealso{
  \code{\link{sample}}
}
\keyword{xt}

