\name{xtsubset}
\alias{xtsubset}
\title{Subset a data frame while preserving its \code{\link{xtdata}} attribute}
\usage{
  xtsubset(data, formula, ...)
}
\arguments{
  \item{data}{a data frame with the \code{\link{xtdata}}
  attribute}

  \item{formula}{a logical formula to subset to}

  \item{...}{other methods passed to \code{\link{subset}}}
}
\value{
  a data frame
}
\description{
  A wrapper of the \code{\link{subset}} function that
  preserves the \code{\link{xtdata}} attribute. Not yet
  usable, sorry.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
QOG = xtsubset(qog.ts.demo, cname \%in\% c("Bangladesh", "Pakistan", "India"))
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link{xtdata}}, \code{\link{subset}}
}

