\name{xtacf}
\alias{xtacf}
\title{Plot autocorrelation functions for cross-sectional time series}
\usage{
  xtacf(data, variable, name = "acf > 0",
    type = "correlation")
}
\arguments{
  \item{data}{a data frame carrying an \code{\link{xtdata}}
  attribute.}

  \item{variable}{the variable to plot.}

  \item{name}{the label for ACF > 0 [TODO: add real
  documentation here].}

  \item{type}{the type of autocorrelation to plot. Defaults
  to \code{correlation}. [TODO: add real documentation
  here].}
}
\value{
  a half-baked plot
}
\description{
  Plot ACF or PACF. Work in progress, does not work yet,
  sorry!
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\keyword{graphics}
\keyword{ts}
\keyword{xt}

