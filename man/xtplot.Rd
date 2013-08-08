\name{xtplot}
\alias{xtarea}
\alias{xtline}
\alias{xtplot}
\alias{xtpoint}
\alias{xtstep}
\title{xtplot}
\usage{
  xtplot(data, variable, t = NULL, name = "")
}
\arguments{
  \item{data}{a data frame with an \code{\link{xtdata}}
  attribute.}

  \item{variable}{the variable to plot.}

  \item{t}{the time period to plot. Defaults to
  \code{NULL}, which plots the full data.}

  \item{name}{the name to give the color scale.}
}
\description{
  Plot function for data frames with an
  \code{\link{xtdata}} attribute.
}
\examples{
xtplot(xtsubset(qog.ts.demo, year > 1995 & unna_pop > 10^8), "wdi_hec")
}

