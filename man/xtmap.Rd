\name{xtmap}
\alias{xtmap}
\title{Plot country-level maps}
\usage{
  xtmap(data, variable, t = NULL, continents = NULL,
    regions = NULL, name = "", title = NULL,
    quantize = FALSE, quantize.t = FALSE, iso3n = NULL,
    simplify = NULL, text.size = 12, ncol = NULL,
    nrow = NULL, ...)
}
\arguments{
  \item{data}{a data frame.}

  \item{variable}{a variable name.}

  \item{t}{the time period to plot from, as specified in
  the \code{\link{xtdata}} attribute of the data. If the
  data carries an \code{\link{xtdata}} attribute but
  \code{t} is left unspecified, the maximal value of the
  time period is used. See 'Details'.}

  \item{ncol}{if the plot is faceted by time periods, the
  number of facet columns}

  \item{nrow}{if the plot is faceted by time periods, the
  number of facet rows}

  \item{continents}{a vector of continent names to subset
  the map to.}

  \item{regions}{a vector of region names to subset the map
  to.}

  \item{name}{a name to give to the color scale}

  \item{title}{a title to give to the map}

  \item{quantize}{how many quantiles to cut the variable
  into. Defauls to \code{0}, which leaves \code{variable}
  unaffected.}

  \item{quantize.t}{how many time periods to cut the time
  variable into. It works, but the results are very
  strange.}

  \item{text.size}{the size for text elements.}

  \item{iso3n}{the ISO-3N variable name, if you are using
  the function on cross-sectional data (which will return a
  warning), or if you are overriding the
  \code{\link{xtdata}} attribute of the data frame (which
  also returns a warning).}

  \item{simplify}{the threshold of points under which to
  remove a geographic subregion. Set to something like
  \code{30} to remove islands and overseas areas. More of a
  bug than a feature. Defaults to \code{NULL}, which leaves
  the map intact.}

  \item{...}{other arguments passed to
  \code{\link[ggplot2]{map_data}} or
  \code{\link[maps]{map}}.}
}
\value{
  a \code{ggplot2} object
}
\description{
  Function to plot country-level maps. Requires the
  \code{countrycode}, \code{ggplot2}, \code{mapproj} and
  \code{maps} packages.
}
\details{
  The function is intended as a helper to map country-year
  data. It falls back to mapping the data as a
  cross-section if the data carries no \code{\link{xtdata}}
  attribute or if \code{t} is left unspecified, in which
  case it will map the values of \code{variable} for the
  most recent time period (usually years).

  When the data carries an \code{\link{xtdata}} attribute
  and \code{t} is specified, the function returns facet
  maps of the data, by time period. Use \code{ncol} and
  \code{nrow} to arrange the disposition of the maps. If
  \code{quantize.t} is specified, the time period is cut to
  quantiles and the mean value of \code{variable} are
  plotted, in order to plot things like average values of a
  variable over several decades.
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
      iso3n = "ccode") +
  geom_polygon(color = "grey25") +
  scale_fill_brewer("", palette = "Accent")
# Grayscale version.
xtmap(subset(qog.cs.demo, ccode != 643), "chga_hinst", continent = "Asia",
      iso3n = "ccode") +
  geom_polygon(color = "white") +
  scale_fill_grey("")
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
  \code{\link[ggplot2]{map_data}},
  \code{\link[ggplot2]{map_data}}, \code{\link[maps]{map}}
}
\keyword{graphics}
\keyword{xt}

