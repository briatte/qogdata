\name{xtmap}
\alias{xtmap}
\title{Map \code{xtdata} variables}
\usage{
  xtmap(data, variable, t = NULL, continents = NULL,
    regions = NULL, name = "", title = NULL,
    quantize = FALSE, text.size = 12, iso3n = NULL, ...)
}
\arguments{
  \item{data}{the QOG data frame. The function requires the
  \code{ccode} variable from the QOG \code{std} and
  \code{soc} datasets, as well as the \code{countrycode},
  \code{ggplot2} and \code{maps} packages.}

  \item{variable}{the QOG variable name to colour the map
  with, in quotes.}

  \item{continents}{a vector of continent names to subset
  the map to.}

  \item{regions}{a vector of region names to subset the map
  to.}

  \item{name}{the legend name}

  \item{title}{the map title}

  \item{quantize}{whether to cut the variable into
  quantiles.}

  \item{text.size}{the size for text elements.}

  \item{t}{the time period to plot from. The maximal value
  is used by default, as with 'most recent year' in
  country-year data.}

  \item{iso3n}{the ISO-3N variable name, if you are using
  the function on cross-sectional data (which will return a
  warning).}

  \item{...}{other arguments passed to \code{map_data}.}
}
\description{
  Function to plot maps of Quality of Government (QOG)
  data. Requires the \code{ggplot2} and \code{maps}
  packages.
}
\examples{
# Load QOG demo datasets.
data(qog.demo)
# Fertility rates in Africa, most recent year.
xtmap(qog.ts.demo, "wdi_fr", continent = "Africa")
# Fertility rates in Africa, 1995.
xtmap(qog.ts.demo, "wdi_fr", 1995, continent = "Africa")
# Political regimes in Asia, excluding Russia, using cross-sectional data.
xtmap(subset(qog.cs.demo, ccode != 643), "chga_hinst", continent = "Asia",
      iso3n = "ccode")
# Education levels in Central America, using cross-sectional data.
xtmap(qog.cs.demo, "bl_asy25mf", quantize = 3,
      region = c("Central America", "South America"),
      iso3n = "ccode") +
      scale_fill_brewer("", palette = "Blues")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
}

