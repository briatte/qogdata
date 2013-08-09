\name{xtplot}
\alias{xtarea}
\alias{xtline}
\alias{xtplot}
\alias{xtpoint}
\alias{xtstep}
\title{xtplot}
\usage{
  xtplot(data, variable, name = "")
}
\arguments{
  \item{data}{a data frame with an \code{\link{xtdata}}
  attribute.}

  \item{variable}{the variable to plot.}

  \item{name}{the name to give the color scale.}
}
\description{
  Plot function for data frames with an
  \code{\link{xtdata}} attribute.
}
\examples{
# Population in countries with a population over 100,000 people.
xtplot(xtsubset(qog.ts.demo, unna_pop > 10^8), "unna_pop") + geom_area()
# Health care expenditure per capita in a random country sample.
xtplot(xtsample(qog.ts.demo, 12), "wdi_hec", "line") +
  geom_smooth() + geom_point() +
  xlim(1995, 2010)
}

