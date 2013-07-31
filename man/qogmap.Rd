\name{qogmap}
\alias{qogmap}
\title{qogmap - Map Quality of Government data}
\usage{
  qogmap(data, variable, continents = NULL, regions = NULL,
    name = "", title = NULL, quantize = FALSE,
    text.size = 12, ...)
}
\arguments{
  \item{data}{the QOG data frame. The function requires the
  \code{ccodealp} variable.}

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

  \item{...}{other arguments passed to \code{map_data}.}
}
\description{
  Function to plot maps of Quality of Government (QOG)
  data. Requires the \code{ggplot2} and \code{maps}
  packages.
}
\examples{
# Fertility rates in Africa.
qogmap(qogdata(tempfile(), variables = "wdi_fr"),
       variable = "wdi_fr", continent = "Africa")
# Political regimes in Asia.
qogmap(subset(QOG.cs, cname != "Russia"),
       "chga_hinst", continent = "Asia")
# Education levels in Central America.
qogmap(qogdata(tempfile(), variables = "bl_asy25mf"),
       variable = "bl_asy25mf", quantize = 3,
       region = c("Central America", "South America")) +
       scale_fill_brewer("", palette = "Blues")
}
\author{
  Francois Briatte \email{f.briatte@ed.ac.uk}
}
\seealso{
  \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
}

